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
  }

  let cc =
    {
      exe = "clang";
      args = [];
      out_flag = "-o";
      obj_flag = "-c";
      ext = String_set.of_list [ "c" ];
    }

  let cxx =
    { cc with exe = "clang++"; ext = String_set.of_list [ "cpp"; "cc" ] }

  let compile_obj t mgr ~sw ~output args =
    let st =
      try
        Option.some
        @@ ( Eio.Path.stat ~follow:true output.Object_file.path,
             Eio.Path.stat ~follow:true output.source.path )
      with _ -> None
    in
    match st with
    | Some (obj, src) when obj.mtime > src.mtime ->
        Eio.traceln "cached: %s" (Eio.Path.native_exn output.source.path);
        None
    | _ ->
        Eio.traceln "compiling: %s" (Eio.Path.native_exn output.source.path);
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
        Some (Eio.Process.spawn mgr cmd ~sw)

  let link t mgr objs ~output args =
    Util.mkparent output;
    let cmd = t.exe :: t.args in
    let objs =
      List.map (fun f -> Eio.Path.native_exn f.Object_file.path) objs
    in
    let args = cmd @ args @ objs @ [ t.out_flag; Eio.Path.native_exn output ] in
    Eio.Process.run mgr args
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String.compare a.Compiler.exe b.Compiler.exe
  end)

  let default = of_list Compiler.[ cc; cxx ]
end

module Build = struct
  type t = {
    env : Eio_posix.stdenv;
    source : path;
    build : path;
    compiler_index : (string, Compiler.t) Hashtbl.t;
    compilers : Compiler_set.t;
    linker : Compiler.t;
    ignore : Path_set.t;
    mutable output : path;
    mutable detect_files : String_set.t;
    mutable source_files : Source_file.t list;
    mutable flags : Flags.t;
  }

  let add_compile_flags t = Flags.add_compile_flags t.flags
  let add_link_flags t = Flags.add_link_flags t.flags
  let obj_path t = Eio.Path.(t.build / "obj")

  let v ?build ?flags ?(linker = Compiler.cc) ?compilers ?detect ?(ignore = [])
      ~source ~output env =
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
    let detect_files =
      String_set.of_list @@ Option.value ~default:[ "c" ] detect
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
      detect_files;
      flags = Option.value ~default:(Flags.v ()) flags;
      output;
      ignore = Path_set.of_list ignore;
    }

  let detect_source_files t =
    let ext' = t.detect_files in
    let rec inner path =
      let files = Eio.Path.read_dir path in
      let files =
        List.filter_map
          (fun file ->
            let f = Eio.Path.(path / file) in
            if Eio.Path.is_directory f then
              let () = if not (String.equal file "zenon-build") then inner f in
              None
            else if String_set.mem (Util.ext f) ext' then Some (Source_file.v f)
            else None)
          files
      in
      t.source_files <- t.source_files @ files
    in
    inner t.source

  let parse_compile_flags t f =
    Eio.Path.with_lines f @@ fun lines ->
    let lines = Seq.map String.trim lines |> List.of_seq in
    add_compile_flags t lines

  let detect_flags_from_compile_flags t =
    let f = Eio.Path.(t.source / "compile_flags.txt") in
    if Eio.Path.is_file f then parse_compile_flags t f
    else if Eio.Path.is_file Eio.Path.(t.env#cwd / "compile_flags.txt") then
      parse_compile_flags t Eio.Path.(t.env#cwd / "compile_flags.txt")

  let detect t =
    detect_source_files t;
    detect_flags_from_compile_flags t

  let add_source_file t ?flags path =
    let path = Eio.Path.(t.source / path) in
    t.source_files <- Source_file.v ?flags path :: t.source_files

  let clean t = Eio.Path.rmtree ~missing_ok:true t.build
  let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)

  let run t =
    let objs = ref [] in
    Eio.Switch.run @@ fun sw ->
    let pool =
      Eio.Executor_pool.create ~sw
        ~domain_count:(Domain.recommended_domain_count ())
        t.env#domain_mgr
    in
    let link_flags = ref t.flags in
    let visited = ref Path_set.empty in
    let tasks =
      List.filter_map
        (fun source ->
          if
            Path_set.mem source.Source_file.path !visited
            || Path_set.mem source.path t.ignore
          then None
          else
            let obj = Object_file.of_source ~build_dir:(obj_path t) source in
            let compiler =
              Hashtbl.find_opt t.compiler_index (Source_file.ext source)
            in
            link_flags := Flags.concat !link_flags source.flags;
            match compiler with
            | None ->
                let source_name = Eio.Path.native_exn source.Source_file.path in
                Eio.traceln "no compiler found for %s" source_name;
                failwith ("no compiler found for " ^ source_name)
            | Some c ->
                objs := obj :: !objs;
                Option.some
                @@ Eio.Executor_pool.submit_fork ~sw pool ~weight:1.0
                @@ fun () ->
                Eio.Switch.run @@ fun sw ->
                let res =
                  Compiler.compile_obj c t.env#process_mgr ~output:obj ~sw
                    t.flags.compile
                in
                visited := Path_set.add source.path !visited;
                Option.iter Eio.Process.await_exn res)
        t.source_files
    in
    List.iter (fun task -> Eio.Promise.await_exn task) tasks;
    Eio.traceln "linking output: %s" (Eio.Path.native_exn t.output);
    Compiler.link t.linker t.env#process_mgr !objs ~output:t.output
      !link_flags.link
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
    let cxx = { default with name = Some "c++" }

    let find_compiler = function
      | "c" | "cc" -> Some Compiler.cc
      | "c++" | "cxx" -> Some Compiler.cxx
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
              obj_flag = t.obj_flag;
            }
  end

  module Build_config = struct
    type t = {
      path : string option; [@default None]
      output : string;
      compilers : Compiler_config.t list; [@default [ Compiler_config.c ]]
      linker : Compiler_config.t; [@default Compiler_config.c]
      files : string list; [@default []]
      detect_files : string list; [@default []]
      cflags : string list; [@default []]
      ldflags : string list; [@default []]
    }
    [@@deriving yaml]

    let append a b =
      {
        path = a.path;
        output = a.output;
        compilers = a.compilers @ b.compilers;
        linker = a.linker;
        files = a.files @ b.files;
        detect_files = a.detect_files @ b.detect_files;
        cflags = a.cflags @ b.cflags;
        ldflags = a.ldflags @ b.ldflags;
      }
  end

  type t = { build : Build_config.t list } [@@deriving yaml]

  let read_file path =
    try
      let s = Eio.Path.load path in
      let y = Yaml.of_string_exn s in
      of_yaml y
    with exn -> Error (`Msg (Printexc.to_string exn))

  let rec read_file_or_default path =
    if Eio.Path.is_file path then read_file path
    else if Eio.Path.is_directory path then
      read_file_or_default Eio.Path.(path / "zenon.yaml")
    else
      Ok
        {
          build =
            [
              Build_config.
                {
                  output = "a.out";
                  path = None;
                  compilers = [ Compiler_config.c ];
                  linker = Compiler_config.c;
                  files = [];
                  detect_files = [ "c" ];
                  cflags = [];
                  ldflags = [];
                };
            ];
        }

  let init ~env path config =
    List.map
      (fun config ->
        let linker = Compiler_config.compiler config.Build_config.linker in
        let compilers = List.map Compiler_config.compiler config.compilers in
        let flags =
          Flags.v ~compile:config.Build_config.cflags ~link:config.ldflags ()
        in
        let build =
          Build.v ~flags ~linker ~compilers
            ~output:Eio.Path.(path / config.output)
            ~source:
              (match config.path with
              | None -> path
              | Some p -> Eio.Path.(path / p))
            ~detect:config.detect_files env
        in
        Build.detect build;
        List.iter (fun file -> Build.add_source_file build file) config.files;
        build)
      config.build

  let load ~env path =
    let config = read_file_or_default path in
    match config with
    | Ok config -> Ok (init ~env path config)
    | Error e -> Error e
end
