type path = Eio.Fs.dir_ty Eio.Path.t

module String_set = Set.Make (String)

module Path_set = Set.Make (struct
  type t = path

  let compare a b =
    let a = Eio.Path.native_exn a in
    let b = Eio.Path.native_exn b in
    String.compare a b
end)

module Util = struct
  let ext path =
    let s =
      Eio.Path.split path |> Option.map snd |> Option.value ~default:""
      |> Filename.extension
    in
    if String.length s > 0 then String.sub s 1 (String.length s - 1) else ""

  let with_ext path ext =
    let a, b = Eio.Path.split path |> Option.get in
    let c = Filename.remove_extension b ^ "." ^ ext in
    Eio.Path.(a / c)
end

module Flags = struct
  type t = { mutable compile : string list; mutable link : string list }

  let v ?(compile = []) ?(link = []) () = { compile; link }
  let add_compile_flags t flags = t.compile <- t.compile @ flags
  let add_link_flags t flags = t.link <- t.link @ flags
  let concat a b = { compile = a.compile @ b.compile; link = a.link @ b.link }
end

module Source_file = struct
  type t = { path : path; flags : Flags.t }

  let v ?flags path = { path; flags = Option.value ~default:(Flags.v ()) flags }
  let ext { path; _ } = Util.ext path
end

module Object_file = struct
  type t = { source : Source_file.t; path : path; flags : Flags.t }

  let v ?flags ~source path =
    { source; path; flags = Option.value ~default:(Flags.v ()) flags }

  let of_source ?flags source =
    let obj_file = Util.with_ext source.Source_file.path "o" in
    v ?flags ~source @@ obj_file
end

module Compiler = struct
  type t = {
    exe : string;
    args : string list;
    ext : String_set.t;
    out_flag : string;
    obj_flag : string;
    obj_map : string option;
  }

  let cc =
    {
      exe = "clang";
      args = [];
      out_flag = "-o";
      obj_flag = "-c";
      ext = String_set.of_list [ "c" ];
      obj_map = None;
    }

  let cxx =
    { cc with exe = "clang++"; ext = String_set.of_list [ "cpp"; "cc" ] }

  let ocaml =
    {
      exe = "ocamlfind";
      args = [ "ocamlopt" ];
      out_flag = "-o";
      obj_flag = "-c";
      obj_map = Some "cmx";
      ext = String_set.of_list [ "ml" ];
    }

  let compile_obj t mgr ~output args =
    let cmd = (t.exe :: t.args) @ args in
    Eio.Process.spawn mgr
      (cmd
      @ [
          t.obj_flag;
          Eio.Path.native_exn output.Object_file.source.path;
          t.out_flag;
          Eio.Path.native_exn output.Object_file.path;
        ])

  let link t mgr objs ~output args =
    let cmd = t.exe :: t.args in
    let objs =
      List.map
        (fun f ->
          Eio.Path.native_exn
            (if String_set.mem (Source_file.ext f.Object_file.source) t.ext then
               match t.obj_map with
               | Some ext -> Util.with_ext f.Object_file.path ext
               | None -> f.path
             else f.path))
        objs
    in
    Eio.Process.run mgr
      (cmd @ objs @ args @ [ t.out_flag; Eio.Path.native_exn output ])
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String.compare a.Compiler.exe b.Compiler.exe
  end)
end

module Build = struct
  type t = {
    env : Eio_posix.stdenv;
    source : path;
    build : path;
    compiler_index : (string, Compiler.t) Hashtbl.t;
    compilers : Compiler_set.t;
    output : path;
    mutable source_files : Source_file.t list;
    mutable flags : Flags.t;
  }

  let default_compilers = Compiler.[ cc; cxx; ocaml ]

  let v ?build ?flags ?(compilers = default_compilers) ~source ~output env =
    let compilers = Compiler_set.of_list compilers in
    let build =
      match build with
      | None -> Eio.Path.(source / ".build")
      | Some path -> path
    in
    let compiler_index = Hashtbl.create 8 in
    Compiler_set.iter
      (fun c ->
        String_set.iter
          (fun ext -> Hashtbl.replace compiler_index ext c)
          c.Compiler.ext)
      compilers;
    {
      env;
      source;
      build;
      compiler_index;
      compilers;
      source_files = [];
      flags = Option.value ~default:(Flags.v ()) flags;
      output;
    }

  let detect_source_files t ext =
    let ext' = String_set.of_list ext in
    let rec inner path =
      let files = Eio.Path.read_dir path in
      let files =
        List.filter_map
          (fun file ->
            let f = Eio.Path.(path / file) in
            if Eio.Path.is_directory f then
              let () = inner f in
              None
            else if Eio.Path.is_file f && String_set.mem (Util.ext f) ext' then
              Some (Source_file.v f)
            else None)
          files
      in
      t.source_files <- t.source_files @ files
    in
    inner t.source

  let add_source_file t ?flags path =
    let path = Eio.Path.(t.source / String.concat Filename.dir_sep path) in
    t.source_files <- Source_file.v ?flags path :: t.source_files

  let run t =
    let objs = ref [] in
    Eio.Switch.run @@ fun sw ->
    let tasks =
      List.map
        (fun source ->
          let obj = Object_file.of_source source in
          let compiler =
            Hashtbl.find_opt t.compiler_index (Source_file.ext source)
          in
          match compiler with
          | None ->
              print_endline
                ("no compiler found for " ^ Eio.Path.native_exn source.path);
              failwith "No compiler"
          | Some c ->
              print_endline
                ("found compiler for "
                ^ Eio.Path.native_exn source.path
                ^ ": " ^ c.Compiler.exe);
              objs := obj :: !objs;
              Compiler.compile_obj c t.env#process_mgr ~output:obj ~sw
                t.flags.compile)
        t.source_files
    in
    List.iter (fun task -> Eio.Process.await_exn task) tasks;
    Compiler.link Compiler.ocaml t.env#process_mgr !objs ~output:t.output
      t.flags.link
end

module Workspace = struct
  type t = { builds : Build.t list; flags : Flags.t }

  let v ?flags builds =
    { builds; flags = Option.value ~default:(Flags.v ()) flags }

  let run { builds; flags; _ } =
    List.iter
      (fun b ->
        let b = Build.{ b with flags = Flags.concat flags b.flags } in
        Build.run b)
      builds
end
