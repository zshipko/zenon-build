type path = Eio.Fs.dir_ty Eio.Path.t

module String_set = Set.Make (String)

module Path_set = Set.Make (struct
  type t = path

  let compare a b =
    let a = Eio.Path.native_exn a in
    let b = Eio.Path.native_exn b in
    String.compare a b
end)

let log fmt = Fmt.epr (fmt ^^ "\n%!")

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

  let relative_to base a =
    let prefix = Eio.Path.native_exn base in
    let a = Eio.Path.native_exn a in
    if String.starts_with ~prefix a then
      let prefix_len = String.length prefix in
      String.sub a (prefix_len + 1) (String.length a - prefix_len - 1)
    else a
end

module Flags = struct
  type t = { mutable compile : string list; mutable link : string list }

  let v ?(compile = []) ?(link = []) () = { compile; link }
  let add_compile_flags t flags = t.compile <- t.compile @ flags
  let add_link_flags t flags = t.link <- t.link @ flags
  let concat a b = { compile = a.compile @ b.compile; link = a.link @ b.link }
end

module Source_file = struct
  type t = { path : path; flags : Flags.t; root : path }

  let v ?flags ~root path =
    { path; flags = Option.value ~default:(Flags.v ()) flags; root }

  let ext { path; _ } = Util.ext path
end

module Object_file = struct
  type t = { source : Source_file.t; path : path; flags : Flags.t }

  let v ?flags ~source path =
    { source; path; flags = Option.value ~default:(Flags.v ()) flags }

  let of_source ?flags ~build_dir source =
    let obj_file = Util.with_ext source.Source_file.path "o" in
    let obj_file =
      Eio.Path.(build_dir / Util.relative_to source.root obj_file)
    in
    v ?flags ~source @@ obj_file
end

module Compiler = struct
  type t = {
    command : string list;
    ext : String_set.t;
    out_flag : string;
    obj_flag : string;
  }

  let cc =
    {
      command = [ "clang" ];
      out_flag = "-o";
      obj_flag = "-c";
      ext = String_set.of_list [ "c" ];
    }

  let cxx =
    {
      cc with
      command = [ "clang++" ];
      ext = String_set.of_list [ "cc"; "cpp" ];
    }

  let zig_cc =
    {
      command = [ "zig"; "cc" ];
      out_flag = "-o";
      obj_flag = "-c";
      ext = String_set.of_list [ "c" ];
    }

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
        log "• CACHE %s (%s)"
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        None
    | _ ->
        log "• BUILD %s -> %s"
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        Util.mkparent output.Object_file.path;
        let cmd =
          t.command @ args
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
    let objs =
      List.map (fun f -> Eio.Path.native_exn f.Object_file.path) objs
    in
    let args =
      t.command @ args @ objs @ [ t.out_flag; Eio.Path.native_exn output ]
    in
    Eio.Process.run mgr args
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b =
      List.compare String.compare a.Compiler.command b.Compiler.command
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
    script : string option;
    after : string option;
    name : string;
    mutable depends : String_set.t;
    mutable disable_cache : bool;
    mutable output : path option;
    mutable detect_files : String_set.t;
    mutable source_files : Source_file.t list;
    mutable flags : Flags.t;
    mutable compiler_flags : (string, Flags.t) Hashtbl.t;
  }

  let add_compile_flags t = Flags.add_compile_flags t.flags
  let add_link_flags t = Flags.add_link_flags t.flags
  let obj_path t = Eio.Path.(t.build / "obj")

  let v ?build ?script ?after ?flags ?(linker = Compiler.cc) ?compilers
      ?(compiler_flags = []) ?detect ?(ignore = []) ?(disable_cache = false)
      ?output ?(depends = []) ~source ~name env =
    let compilers =
      match compilers with
      | None -> Compiler_set.default
      | Some compilers -> Compiler_set.of_list compilers
    in
    let build =
      match build with
      | None -> Eio.Path.(env#cwd / "zenon-build")
      | Some path -> path
    in
    let detect_files =
      String_set.of_list @@ Option.value ~default:[ "c" ] detect
    in
    Eio.Path.mkdirs ~exists_ok:true build ~perm:0o755;
    let compiler_flags = Hashtbl.of_seq (List.to_seq compiler_flags) in
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
      detect_files;
      script;
      after;
      source_files = [];
      flags = Option.value ~default:(Flags.v ()) flags;
      output;
      ignore = Path_set.of_list ignore;
      name;
      compiler_flags;
      disable_cache;
      depends = String_set.of_list depends;
    }

  let detect_source_files t =
    let ext' = t.detect_files in
    let known =
      ref
        (Path_set.of_list
           (List.map (fun x -> x.Source_file.path) t.source_files))
    in
    let rec inner path =
      let files = Eio.Path.read_dir path in
      let files =
        List.filter_map
          (fun file ->
            let f = Eio.Path.(path / file) in
            if Path_set.mem f !known then None
            else if Eio.Path.is_directory f then
              let () = if not (String.equal file "zenon-build") then inner f in
              None
            else if String_set.mem (Util.ext f) ext' then
              Some (Source_file.v ~root:t.source f)
            else None)
          files
      in
      List.iter (fun f -> known := Path_set.add f.Source_file.path !known) files;
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
    t.source_files <-
      t.source_files @ [ Source_file.v ?flags ~root:t.source path ]

  let add_source_files t files = t.source_files <- t.source_files @ files
  let clean t = Eio.Path.rmtree ~missing_ok:true t.build
  let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)

  let run t =
    let objs = ref [] in
    let pool = Eio.Semaphore.make (Domain.recommended_domain_count ()) in
    log "◎ RUN %s" t.name;
    let link_flags = ref t.flags in
    let visited = ref Path_set.empty in
    let () =
      Option.iter
        (fun s ->
          log "• SCRIPT %s" s;
          match Sys.command s with
          | 0 -> ()
          | n -> failwith (Printf.sprintf "script failed with exit code: %d" n))
        t.script
    in
    detect t;
    let tasks =
      List.filter_map
        (fun source ->
          if
            (not t.disable_cache)
            && (Path_set.mem source.Source_file.path !visited
               || Path_set.mem source.path t.ignore)
          then None
          else
            let obj =
              Object_file.of_source ~build_dir:Eio.Path.(t.build / "obj") source
            in
            let compiler =
              Hashtbl.find_opt t.compiler_index (Source_file.ext source)
            in
            let () = link_flags := Flags.concat !link_flags source.flags in
            match compiler with
            | None ->
                let source_name = Eio.Path.native_exn source.Source_file.path in
                log "ERROR: no compiler found for %s" source_name;
                failwith ("no compiler found for " ^ source_name)
            | Some c ->
                objs := obj :: !objs;
                Option.some @@ Eio.Switch.run
                @@ fun sw ->
                Eio.Fiber.fork_promise ~sw @@ fun () ->
                Eio.Semaphore.acquire pool;
                Fun.protect ~finally:(fun () -> Eio.Semaphore.release pool)
                @@ fun () ->
                Eio.Switch.run @@ fun sw ->
                let flags =
                  Hashtbl.find_opt t.compiler_flags (Source_file.ext source)
                  |> Option.value ~default:(Flags.v ())
                in
                let flags = Flags.concat source.flags flags in
                let () = link_flags := Flags.concat !link_flags flags in
                let res =
                  Compiler.compile_obj c t.env#process_mgr ~output:obj ~sw
                    (Flags.concat t.flags flags).compile
                in
                visited := Path_set.add source.path !visited;
                Option.iter Eio.Process.await_exn res)
        t.source_files
    in
    List.iter (fun task -> Eio.Promise.await_exn task) tasks;
    let () =
      Option.iter
        (fun s ->
          log "• SCRIPT %s" s;
          match Sys.command s with
          | 0 -> ()
          | n -> failwith (Printf.sprintf "script failed with exit code: %d" n))
        t.after
    in
    Option.iter
      (fun output ->
        if not (List.is_empty t.source_files) then
          log "⁕ LINK %s" (Eio.Path.native_exn output);
        Compiler.link t.linker t.env#process_mgr !objs ~output !link_flags.link)
      t.output
end

type value =
  | Build of Build.t
  | Src of Source_file.t
  | Obj of Object_file.t
  | Output of path

let value_id = function
  | Build b -> "build:" ^ b.name
  | Src s -> "src:" ^ Eio.Path.native_exn s.path
  | Obj s -> "obj:" ^ Eio.Path.native_exn s.path
  | Output s -> "out:" ^ Eio.Path.native_exn s

type cmd =
  | Script of string * path option
  | Compiler of Compiler.t * Flags.t option

let cmd_id = function
  | Script (b, output) ->
      "script:"
      ^ (Digest.string
           ((Option.map Eio.Path.native_exn output |> Option.value ~default:"")
           ^ b)
        |> Digest.to_hex)
  | Compiler (c, Some f) ->
      "compiler:"
      ^ String.concat "_" c.command
      ^ String.concat "_" f.Flags.compile
      ^ String.concat "_" f.Flags.link
  | Compiler (c, None) -> "compiler:" ^ String.concat "_" c.command

module Make = Graph.Imperative.Digraph.ConcreteLabeled (struct
  type t = value

  let compare a b = String.compare (value_id a) (value_id b)
  let hash t = Hashtbl.hash (value_id t)
  let equal a b = compare a b = 0
end)

module G = Make (struct
  type t = cmd option

  let default = None

  let compare a b =
    Option.compare String.compare (Option.map cmd_id a) (Option.map cmd_id b)
end)

module Plan = struct
  type t = { graph : G.t }

  let v () = { graph = G.create () }

  let build t (b : Build.t) =
    Build.detect b;
    let build_node = Build b in
    let output_node = Option.map (fun x -> Output x) b.output in
    G.add_vertex t.graph build_node;
    Option.iter (G.add_vertex t.graph) output_node;
    List.iter
      (fun src ->
        let src_node = Src src in
        G.add_vertex t.graph src_node;
        let e =
          match b.script with
          | Some s ->
              Some
                (Script
                   (s, if List.is_empty b.source_files then b.output else None))
          | None -> None
        in
        let obj_node =
          Obj (Object_file.of_source ~build_dir:Eio.Path.(b.build / "obj") src)
        in
        G.add_edge_e t.graph @@ G.E.create build_node e src_node;
        let ext = Source_file.ext src in
        let compiler =
          Compiler_set.find_first
            (fun c -> String_set.mem ext c.Compiler.ext)
            b.compilers
        in
        G.add_edge_e t.graph
        @@ G.E.create src_node
             (Some
                (Compiler
                   ( compiler,
                     Some
                       (Flags.concat
                          (Hashtbl.find_opt b.compiler_flags ext
                          |> Option.value ~default:(Flags.v ()))
                          b.flags) )))
             obj_node;
        Option.iter
          (fun node -> G.add_edge_e t.graph @@ G.E.create obj_node e node)
          output_node)
      b.source_files

  let run_build t (b : Build.t) =
    log "◎ RUN %s" b.name;
    Option.iter
      (fun s ->
        log "• SCRIPT %s" s;
        match Sys.command s with
        | 0 -> ()
        | n -> failwith (Printf.sprintf "script failed with exit code: %d" n))
      b.script;
    Build.detect b;
    let link_flags = ref b.flags in
    let max = Domain.recommended_domain_count () in
    let pool = Eio.Semaphore.make max in
    let objs =
      G.fold_succ_e
        (fun edge objs ->
          Eio.Switch.run @@ fun sw ->
          match G.E.dst edge with
          | Src s as v' -> (
              let obj =
                Object_file.of_source ~build_dir:Eio.Path.(b.build / "obj") s
              in
              let obj_node = Obj obj in
              match G.find_edge t.graph v' obj_node |> G.E.label with
              | None -> objs
              | Some edge' -> (
                  match edge' with
                  | Compiler (c, flags) ->
                      let flags = Option.value ~default:(Flags.v ()) flags in
                      Eio.Semaphore.acquire pool;
                      ( Eio.Fiber.fork ~sw @@ fun () ->
                        Fun.protect ~finally:(fun () ->
                            Eio.Semaphore.release pool)
                        @@ fun () ->
                        let task =
                          Compiler.compile_obj c b.env#process_mgr ~output:obj
                            ~sw (Flags.concat b.flags flags).compile
                        in
                        let () = link_flags := Flags.concat !link_flags flags in
                        Option.iter Eio.Process.await_exn task );
                      obj :: objs
                  | _ -> objs))
          | _ -> objs)
        t.graph (Build b) []
    in
    Option.iter
      (fun output ->
        if not (List.is_empty b.source_files) then
          log "⁕ LINK %s" (Eio.Path.native_exn output);
        Compiler.link b.linker b.env#process_mgr objs ~output !link_flags.link)
      b.output;
    Option.iter
      (fun s ->
        log "• SCRIPT %s" s;
        match Sys.command s with
        | 0 -> ()
        | n -> failwith (Printf.sprintf "script failed with exit code: %d" n))
      b.after

  let run_all t builds = List.iter (run_build t) builds
end

module Config = struct
  module Compiler_config = struct
    type t = {
      name : string option; [@default None]
      command : string list; [@default []]
      ext : string list; [@default []]
      out_flag : string; [@default "-o"] [@key "out-flag"]
      obj_flag : string; [@default "-c"] [@key "obj-flag"]
    }
    [@@deriving yaml]

    let default =
      { name = None; command = []; ext = []; out_flag = "-o"; obj_flag = "-c" }

    let c = { default with name = Some "c" }
    let cxx = { default with name = Some "cpp" }

    let find_compiler = function
      | "c" -> Some Compiler.cc
      | "cc" | "cpp" -> Some Compiler.cxx
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
              command = t.command;
              ext = String_set.of_list t.ext;
              out_flag = t.out_flag;
              obj_flag = t.obj_flag;
            }
  end

  module Build_config = struct
    type flags = {
      compile : string list; [@default []]
      link : string list; [@default []]
    }
    [@@deriving yaml]

    module Lang_flags = struct
      type t = {
        lang : string;
        compile : string list; [@default []]
        link : string list; [@default []]
      }
      [@@deriving yaml]
    end

    type t = {
      name : string;
      path : string option; [@default None]
      output : string option; [@default None]
      compilers : Compiler_config.t list; [@default [ Compiler_config.c ]]
      linker : Compiler_config.t; [@default Compiler_config.c]
      files : string list; [@default []]
      detect : string list; [@default []]
      flags : Lang_flags.t list; [@default []]
      script : string option; [@default None]
      after : string option; [@default None]
      disable_cache : bool; [@default false]
    }
    [@@deriving yaml]

    let default =
      {
        name = "default";
        output = Some "a.out";
        path = None;
        compilers = [ Compiler_config.c ];
        linker = Compiler_config.c;
        files = [];
        detect = [ "c" ];
        script = None;
        after = None;
        flags = [];
        disable_cache = false;
      }
  end

  type t = {
    build : Build_config.t list;
    flags : Build_config.Lang_flags.t list; [@default []]
    compilers : Compiler_config.t list; [@default [ Compiler_config.c ]]
  }
  [@@deriving yaml]

  let default =
    {
      build = [ Build_config.default ];
      flags = [];
      compilers = [ Compiler_config.c ];
    }

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
    else Ok default

  let init ~env path t =
    List.map
      (fun config ->
        let linker = Compiler_config.compiler config.Build_config.linker in
        let compilers =
          List.map Compiler_config.compiler (t.compilers @ config.compilers)
        in
        let compiler_flags =
          List.to_seq (t.flags @ config.flags)
          |> Seq.map (fun v ->
                 ( v.Build_config.Lang_flags.lang,
                   Flags.v ~compile:v.compile ~link:v.link () ))
          |> List.of_seq
        in
        let source =
          match config.path with None -> path | Some p -> Eio.Path.(path / p)
        in
        let output =
          Option.map (fun output -> Eio.Path.(source / output)) config.output
        in
        let build =
          Build.v ?script:config.script ?after:config.after ~linker ~compilers
            ~compiler_flags ?output ~source ~detect:config.detect
            ~name:config.name env
        in
        Build.add_source_files build
          (List.map
             (fun file -> Source_file.v ~root:source Eio.Path.(source / file))
             config.files);
        build)
      t.build

  let load ~env path =
    let config = read_file_or_default path in
    match config with
    | Ok config -> Ok (init ~env path config)
    | Error e -> Error e
end
