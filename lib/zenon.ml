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

  let mkparent path =
    let parent = Eio.Path.split path |> Option.map fst in
    Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) parent
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

  let of_source ?flags ~build_dir source =
    let obj_file = Util.with_ext source.Source_file.path "o" in
    let obj_file = Eio.Path.(build_dir / Eio.Path.native_exn obj_file) in
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
    Util.mkparent output.Object_file.path;
    let cmd =
      (t.exe :: t.args) @ args
      @ [
          t.obj_flag;
          Eio.Path.native_exn output.Object_file.source.path;
          t.out_flag;
          Eio.Path.native_exn output.Object_file.path;
        ]
    in
    Eio.Process.spawn mgr cmd

  let link t mgr objs ~output args =
    Util.mkparent output;
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
    let args = cmd @ args @ objs @ [ t.out_flag; Eio.Path.native_exn output ] in
    Eio.Process.run mgr args
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String.compare a.Compiler.exe b.Compiler.exe
  end)

  let default = of_list Compiler.[ cc; cxx; ocaml ]
end

module Build = struct
  type t = {
    env : Eio_posix.stdenv;
    source : path;
    build : path;
    compiler_index : (string, Compiler.t) Hashtbl.t;
    compilers : Compiler_set.t;
    linker : Compiler.t;
    output : path;
    mutable source_files : Source_file.t list;
    mutable flags : Flags.t;
  }

  let add_compile_flags t = Flags.add_compile_flags t.flags
  let add_link_flags t = Flags.add_link_flags t.flags
  let obj_path t = Eio.Path.(t.build / "obj")

  let v ?build ?flags ?(linker = Compiler.cc) ?compilers ~source ~output env =
    let compilers =
      match compilers with
      | None -> Compiler_set.default
      | Some compilers -> Compiler_set.of_list compilers
    in
    let build =
      match build with
      | None -> Eio.Path.(source / "zenon-build")
      | Some path -> path
    in
    Eio.Path.mkdirs ~exists_ok:true build ~perm:0o755;
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
      linker;
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
              let () = if not (String.equal file "zenon-build") then inner f in
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
    let path = Eio.Path.(t.source / path) in
    t.source_files <- Source_file.v ?flags path :: t.source_files

  let clean t = Eio.Path.rmtree ~missing_ok:true t.build
  let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)

  let run t =
    let objs = ref [] in
    Eio.Switch.run @@ fun sw ->
    let link_flags = ref t.flags in
    let tasks =
      List.map
        (fun source ->
          let obj = Object_file.of_source ~build_dir:(obj_path t) source in
          let compiler =
            Hashtbl.find_opt t.compiler_index (Source_file.ext source)
          in
          link_flags := Flags.concat !link_flags source.flags;
          match compiler with
          | None ->
              Eio.traceln "no compiler found for %s"
              @@ Eio.Path.native_exn source.path;
              failwith "No compiler"
          | Some c ->
              Eio.traceln "found compiler for %s: %s"
                (Eio.Path.native_exn source.path)
                c.Compiler.exe;
              objs := obj :: !objs;
              Compiler.compile_obj c t.env#process_mgr ~output:obj ~sw
                t.flags.compile)
        t.source_files
    in
    List.iter (fun task -> Eio.Process.await_exn task) tasks;
    Eio.traceln "linking output: %s" (Eio.Path.native_exn t.output);
    Compiler.link t.linker t.env#process_mgr !objs ~output:t.output
      !link_flags.link
end

module Workspace = struct
  type t = { root : path; mutable builds : Build.t list; flags : Flags.t }

  let v ?flags root builds =
    { builds; root; flags = Option.value ~default:(Flags.v ()) flags }

  let run { builds; flags; _ } =
    List.iter
      (fun b ->
        let b = Build.{ b with flags = Flags.concat flags b.flags } in
        Build.run b)
      builds

  let clean_obj t = List.iter Build.clean_obj t.builds
  let clean t = List.iter Build.clean t.builds
end

module Config = struct
  module Compiler_config = struct
    type t = {
      name : string option; [@default None]
      command : string list; [@default []]
      ext : string list; [@default []]
      out_flag : string; [@default "-o"]
      obj_flag : string; [@default "-c"]
      obj_map : string option; [@default None]
    }
    [@@deriving yaml]

    let default =
      {
        name = None;
        command = [];
        ext = [];
        out_flag = "-o";
        obj_flag = "-c";
        obj_map = None;
      }

    let c = { default with name = Some "c" }
    let ocaml = { default with name = Some "ocaml" }
    let cxx = { default with name = Some "c++" }

    let find_compiler = function
      | "c" | "cc" -> Some Compiler.cc
      | "c++" | "cxx" -> Some Compiler.cxx
      | "ml" | "ocaml" -> Some Compiler.ocaml
      | _ -> None

    let rec compiler t =
      match t.name with
      | Some name -> (
          match find_compiler name with
          | None -> compiler { t with name = None }
          | Some x -> x)
      | None ->
          Compiler.
            {
              exe = List.hd t.command;
              args = List.tl t.command;
              ext = String_set.of_list t.ext;
              out_flag = t.out_flag;
              obj_map = t.obj_map;
              obj_flag = t.obj_flag;
            }
  end

  module Build_config = struct
    type t = {
      name : string option;
      (* import : string list; [@default []] *)
      output : string;
      compilers : Compiler_config.t list;
          [@default
            [ Compiler_config.c; Compiler_config.cxx; Compiler_config.ocaml ]]
      linker : Compiler_config.t; [@default Compiler_config.c]
      files : string list; [@default []]
      detect_files : string list; [@default [ "c" ]]
      cflags : string list; [@default []]
      ldflags : string list; [@default []]
    }
    [@@deriving yaml]

    let append a b =
      {
        name = a.name;
        output = a.output;
        compilers = a.compilers @ b.compilers;
        linker = a.linker;
        files = a.files @ b.files;
        detect_files = a.detect_files @ b.detect_files;
        cflags = a.cflags @ b.cflags;
        ldflags = a.ldflags @ b.ldflags;
      }
  end

  type t = {
    root : string; [@default "."]
    build : Build_config.t list;
    compilers : Compiler_config.t list; [@default []]
    cflags : string list; [@default []]
    ldflags : string list; [@default []]
  }
  [@@deriving yaml]

  let build t ~name ~output ~compilers ~linker ~files ~detect_files ~cflags
      ~ldflags =
    Build_config.
      {
        name;
        output;
        compilers;
        linker;
        files;
        detect_files;
        cflags = t.cflags @ cflags;
        ldflags = t.ldflags @ ldflags;
      }

  let workspace t ~env path =
    let flags = Flags.v ~compile:t.cflags ~link:t.ldflags () in
    let build =
      List.map
        (fun (b : Build_config.t) ->
          let flags =
            Flags.(concat flags (v ~compile:b.cflags ~link:b.ldflags ()))
          in
          let linker = Compiler_config.compiler b.linker in
          let compilers =
            List.map
              (fun c -> Compiler_config.compiler c)
              (t.compilers @ b.compilers)
          in
          let name = Option.value ~default:"." b.name in
          let source = Eio.Path.(path / name) in
          let output = Eio.Path.(source / b.output) in
          let build = Build.v ~flags ~linker ~compilers ~source ~output env in
          Build.detect_source_files build b.detect_files;
          List.iter (Build.add_source_file build) b.files;
          build)
        t.build
    in
    Workspace.v ~flags path build
end
