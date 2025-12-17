include Types
module Util = Util
include Compiler

module Build = struct
  type t = {
    env : Eio_posix.stdenv;
    source : path;
    build : path;
    compiler_index : (string, Compiler.t) Hashtbl.t;
    compilers : Compiler_set.t;
    linker : Compiler.t;
    script : string option;
    after : string option;
    name : string;
    mutable ignore : Re.t list;
    mutable disable_cache : bool;
    mutable output : path option;
    mutable files : Re.t list;
    mutable flags : Flags.t;
    mutable compiler_flags : (string, Flags.t) Hashtbl.t;
  }

  let add_compile_flags t = Flags.add_compile_flags t.flags
  let add_link_flags t = Flags.add_link_flags t.flags
  let obj_path t = Eio.Path.(t.build / "obj")

  let v ?build ?script ?after ?flags ?(linker = Compiler.clang) ?compilers
      ?(compiler_flags = []) ?(files = []) ?(ignore = [])
      ?(disable_cache = false) ?output ~source ~name env =
    let compilers =
      match compilers with
      | None -> Compiler_set.default
      | Some compilers ->
          Compiler_set.union Compiler_set.default
          @@ Compiler_set.of_list compilers
    in
    let build =
      match build with
      | None -> Eio.Path.(env#cwd / "zenon-build")
      | Some path -> path
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
      files = List.map Util.glob_path files;
      script;
      after;
      flags = Option.value ~default:(Flags.v ()) flags;
      output;
      ignore = List.map Util.glob_path ignore;
      name;
      compiler_flags;
      disable_cache;
    }

  let locate_source_files t : Source_file.t list =
    let re = Re.alt t.files |> Re.compile in
    let rec inner path =
      let files = Eio.Path.read_dir path in
      List.fold_left
        (fun (acc : Source_file.t list) file ->
          let f = Eio.Path.(path / file) in
          if Eio.Path.is_directory f then
            if
              not
                (String.equal file "zenon-build"
                || String.equal file ".git" || String.equal file ".jj")
            then inner f @ acc
            else acc
          else if Re.execp re (Eio.Path.native_exn f) then
            Source_file.v ~root:t.source f :: acc
          else acc)
        [] files
    in
    inner t.source

  let parse_compile_flags t f =
    Eio.Path.with_lines f @@ fun lines ->
    let lines = Seq.map String.trim lines |> List.of_seq in
    add_compile_flags t lines

  let flags_from_compile_flags t =
    let f = Eio.Path.(t.source / "compile_flags.txt") in
    if Eio.Path.is_file f then parse_compile_flags t f
    else if Eio.Path.is_file Eio.Path.(t.env#cwd / "compile_flags.txt") then
      parse_compile_flags t Eio.Path.(t.env#cwd / "compile_flags.txt")

  let add_source_file t path = t.files <- t.files @ [ Util.glob_path path ]
  let add_source_files t files = List.iter (fun f -> add_source_file t f) files
  let clean t = Eio.Path.rmtree ~missing_ok:true t.build
  let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)
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
      ^ String.concat "_" (String_set.to_list c.ext)
      ^ String.concat "_" f.Flags.compile
      ^ String.concat "_" f.Flags.link
  | Compiler (c, None) ->
      "compiler:"
      ^ String.concat "_" c.command
      ^ String.concat "_" (String_set.to_list c.ext)

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
    let () =
      if List.is_empty b.files && Option.is_none b.script then
        Build.add_source_file b "*.c"
    in
    let source_files = Build.locate_source_files b in
    let build_node = Build b in
    let output_node = Option.map (fun x -> Output x) b.output in
    G.add_vertex t.graph build_node;
    Option.iter (G.add_vertex t.graph) output_node;
    let ignore = Re.compile (Re.alt b.ignore) in
    List.iter
      (fun src ->
        if Re.execp ignore (Eio.Path.native_exn src.Source_file.path) then ()
        else
          let src_node = Src src in
          G.add_vertex t.graph src_node;
          let e =
            match b.script with
            | Some s ->
                Some
                  (Script
                     (s, if List.is_empty source_files then b.output else None))
            | None -> None
          in
          let obj_node =
            Obj
              (Object_file.of_source ~build_dir:Eio.Path.(b.build / "obj") src)
          in
          G.add_edge_e t.graph @@ G.E.create build_node e src_node;
          let ext = Source_file.ext src in
          let compiler =
            match Hashtbl.find_opt b.compiler_index ext with
            | Some x -> x
            | None ->
                Fmt.failwith
                  "failed to pick compiler for extension %s in compilers: %a \
                   with extensions %a"
                  ext
                  (Fmt.list
                     ~sep:(Fmt.const Fmt.string ", ")
                     (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string))
                  (Compiler_set.to_list b.compilers
                  |> List.map (fun c -> c.Compiler.command))
                  (Fmt.list
                     ~sep:(Fmt.const Fmt.string ", ")
                     (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string))
                  (Compiler_set.to_list b.compilers
                  |> List.map (fun c -> String_set.to_list c.Compiler.ext))
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
      source_files

  let run_build t (b : Build.t) =
    Util.log "◎ RUN %s" b.name;
    Option.iter
      (fun s ->
        Util.log "• SCRIPT %s" s;
        Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
      b.script;
    let link_flags = ref b.flags in
    let max = Domain.recommended_domain_count () in
    let pool = Eio.Semaphore.make max in
    let count = ref 0 in
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
                      incr count;
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
        if !count > 0 then (
          Util.log "⁕ LINK %s" (Eio.Path.native_exn output);
          Compiler.link b.linker b.env#process_mgr objs ~output !link_flags.link
            ~compiler_index:b.compiler_index))
      b.output;
    Option.iter
      (fun s ->
        Util.log "• SCRIPT %s" s;
        Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
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
      obj_ext : string option; [@default None] [@key "obj-ext"]
    }
    [@@deriving yaml]

    let default =
      {
        name = None;
        command = [];
        ext = [];
        out_flag = "-o";
        obj_flag = "-c";
        obj_ext = None;
      }

    let clang = { default with name = Some "clang" }
    let ispc = { default with name = Some "ispc" }
    let clangxx = { default with name = Some "clang++" }
    let ghc = { default with name = Some "ghc" }
    let ocaml = { default with name = Some "ocaml" }

    let find_compiler = function
      | "c" | "clang" -> Some Compiler.clang
      | "clang++" | "c++" | "cxx" | "cc" | "cpp" -> Some Compiler.clangxx
      | "ispc" -> Some Compiler.ispc
      | "hs" | "ghc" -> Some Compiler.ghc
      | "ml" | "ocaml" | "ocamlopt" | "ocamfind" -> Some Compiler.ocaml
      | _ -> None

    let rec compiler compilers t =
      match t.name with
      | Some name -> (
          match find_compiler name with
          | None -> (
              match
                List.find_opt
                  (fun x -> Option.value ~default:"" x.name = name)
                  compilers
              with
              | Some c when not (List.is_empty c.command) ->
                  compiler compilers c
              | _ -> compiler compilers { t with name = None })
          | Some x -> x)
      | None ->
          Compiler.
            {
              command = t.command;
              ext = String_set.of_list t.ext;
              out_flag = t.out_flag;
              obj_flag = t.obj_flag;
              obj_ext = t.obj_ext;
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

    let default_compilers =
      [
        Compiler_config.clang;
        Compiler_config.ispc;
        Compiler_config.ocaml;
        Compiler_config.ghc;
      ]

    type t = {
      name : string option; [@default None]
      path : string option;
      output : string option; [@default None]
      compilers : Compiler_config.t list; [@default default_compilers]
      linker : Compiler_config.t; [@default Compiler_config.clang]
      files : string list; [@default []]
      ignore : string list; [@default []]
      flags : Lang_flags.t list; [@default []]
      script : string option; [@default None]
      after : string option; [@default None]
      disable_cache : bool; [@default false]
      only_if : string option; [@default None] [@key "if"]
    }
    [@@deriving yaml]

    let default =
      {
        name = None;
        output = Some "a.out";
        path = Some ".";
        ignore = [];
        compilers = default_compilers;
        linker = Compiler_config.clang;
        files = [];
        script = None;
        after = None;
        flags = [];
        disable_cache = false;
        only_if = None;
      }
  end

  type t = {
    build : Build_config.t list;
    flags : Build_config.Lang_flags.t list; [@default []]
    compilers : Compiler_config.t list; [@default Build_config.default_compilers]
  }
  [@@deriving yaml]

  let empty = { build = []; flags = []; compilers = [] }

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
    else Ok empty

  let init ~env path t =
    List.filter_map
      (fun config ->
        let ok =
          match config.Build_config.only_if with
          | Some script -> (
              try
                Eio.Process.run env#process_mgr [ "sh"; "-c"; script ];
                true
              with _ -> false)
          | None -> true
        in
        if not ok then
          let () =
            Util.log "! SKIP %s"
              (Option.value
                 ~default:(Option.value ~default:"default" config.path)
                 config.name)
          in
          None
        else
          let linker =
            Compiler_config.compiler t.compilers config.Build_config.linker
          in
          let compilers =
            List.map (Compiler_config.compiler []) t.compilers
            @ List.map (Compiler_config.compiler t.compilers) config.compilers
          in
          let compiler_flags =
            List.to_seq (t.flags @ config.flags)
            |> Seq.map (fun v ->
                   ( v.Build_config.Lang_flags.lang,
                     Flags.v ~compile:v.compile ~link:v.link () ))
            |> List.of_seq
          in
          let source =
            match config.path with
            | None -> path
            | Some p -> Eio.Path.(path / p)
          in
          let output =
            Option.map (fun output -> Eio.Path.(source / output)) config.output
          in
          let name =
            match config.name with
            | Some name -> name
            | None -> (
                match config.path with Some p -> p | None -> "default")
          in
          let build =
            Build.v ?script:config.script ?after:config.after ~linker ~compilers
              ~compiler_flags ?output ~source ~files:config.files ~name
              ~ignore:config.ignore env
          in
          Some build)
      t.build

  let load ~env path =
    let config = read_file_or_default path in
    match config with
    | Ok config -> Ok (init ~env path config)
    | Error e -> Error e
end
