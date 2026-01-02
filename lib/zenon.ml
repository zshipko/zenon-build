include Types
include Compiler
module Util = Util
module Build = Build
module Pkg_config = Pkg_config
module Cmake = Cmake

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
  | Linker of Linker.t

let cmd_id = function
  | Script (b, output) ->
      "script:"
      ^ (Digest.string
           ((Option.map Eio.Path.native_exn output |> Option.value ~default:"")
           ^ b)
        |> Digest.to_hex)
  | Compiler (c, Some f) ->
      "compiler:" ^ c.Compiler.name ^ "_" ^ String.concat "_" f.link
      ^ String.concat "_" f.compile
  | Compiler (c, None) -> "compiler:" ^ c.name
  | Linker link -> "linker:" ^ link.name

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
    let flags = Flags.v ~compile:(Build.compile_flags b) () in
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
                  (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string)
                  (Compiler_set.to_list b.compilers
                  |> List.map (fun c -> c.Compiler.name))
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
                         (Flags.concat flags
                         @@ Flags.concat
                              (Hashtbl.find_opt b.compiler_flags ext
                              |> Option.value ~default:(Flags.v ()))
                              b.flags) )))
               obj_node;
          Option.iter
            (fun node -> G.add_edge_e t.graph @@ G.E.create obj_node e node)
            output_node)
      source_files

  let run_build t ?(execute = false) ?(execute_args = []) (b : Build.t) =
    Util.log "◎ RUN %s" b.name;
    Option.iter
      (fun s ->
        Util.log "• SCRIPT %s" s;
        Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
      b.script;
    let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
    let flags = Flags.v ~compile:(Build.compile_flags b) () in
    let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in
    let link_flags = ref b_flags in
    let max = Domain.recommended_domain_count () in
    let pool = Eio.Semaphore.make max in
    let count = ref 0 in
    let sources, objs =
      G.fold_succ_e
        (fun edge (sources, objs) ->
          match G.E.dst edge with
          | Src s as v' ->
              let obj =
                Object_file.of_source ~build_dir:Eio.Path.(b.build / "obj") s
              in
              (s :: sources, (v', obj) :: objs)
          | _ -> (sources, objs))
        t.graph (Build b) ([], [])
    in
    Eio.Switch.run @@ fun sw ->
    let objs =
      List.filter_map
        (fun (v', obj) ->
          let obj_node = Obj obj in
          match G.find_edge t.graph v' obj_node |> G.E.label with
          | None -> None
          | Some edge' -> (
              match edge' with
              | Compiler (c, flags) ->
                  incr count;
                  let flags = Option.value ~default:(Flags.v ()) flags in
                  Eio.Semaphore.acquire pool;
                  let p =
                    Eio.Fiber.fork_promise ~sw @@ fun () ->
                    Fun.protect ~finally:(fun () -> Eio.Semaphore.release pool)
                    @@ fun () ->
                    let task =
                      Compiler.compile_obj c b.env#process_mgr ~sources
                        ~output:obj ~sw ~build_mtime:b.mtime
                        (Flags.concat b_flags flags)
                    in
                    let () = link_flags := Flags.concat !link_flags flags in
                    Option.iter Eio.Process.await_exn task
                  in
                  Eio.Promise.await_exn p;
                  Some obj
              | _ -> None))
        objs
    in
    Option.iter
      (fun output ->
        if !count > 0 then (
          Util.log "⁕ LINK(%s) %s" b.linker.name (Eio.Path.native_exn output);
          Linker.link b.linker b.env#process_mgr ~objs ~output
            ~flags:!link_flags))
      b.output;
    Option.iter
      (fun s ->
        Util.log "• SCRIPT %s" s;
        Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
      b.after;
    if execute then
      match b.output with
      | None -> Fmt.failwith "target %s has no output" b.name
      | Some exe ->
          Eio.Process.run b.env#process_mgr
            (Eio.Path.native_exn exe :: execute_args)

  let run_all ?execute ?args t builds =
    List.iter (run_build ?execute ?execute_args:args t) builds
end

module Config = struct
  module Compiler_config = struct
    type t = {
      name : string;
      ext : string list; [@default []]
      command : string list option; [@default None]
      link_type : Linker.link_type; [@default Linker.Executable]
    }
    [@@deriving yaml]

    let clang =
      {
        name = "clang";
        ext = [];
        command = None;
        link_type = Linker.Executable;
      }

    let ispc =
      { name = "ispc"; ext = []; command = None; link_type = Linker.Executable }

    let clangxx =
      {
        name = "clang++";
        ext = [];
        command = None;
        link_type = Linker.Executable;
      }

    let ghc =
      { name = "ghc"; ext = []; command = None; link_type = Linker.Executable }

    let find_compiler = function
      | "c" | "clang" -> Some Compiler.clang
      | "clang++" | "c++" | "cxx" | "cc" | "cpp" -> Some Compiler.clangxx
      | "ispc" -> Some Compiler.ispc
      | "ghc" | "hs" | "lhs" -> Some Compiler.ghc
      | _ -> None

    let find_linker = function
      | "c" | "clang" -> Some Linker.clang
      | "shared" -> Some Linker.clang_shared
      | "clang++" | "c++" | "cxx" | "cc" | "cpp" -> Some Linker.clangxx
      | "ar" | "static" -> Some Linker.ar
      | "ghc" | "hs" | "lhs" -> Some Linker.ghc
      | _ -> None

    let compiler t =
      match t.command with
      | Some cmd ->
          Compiler.
            {
              name = t.name;
              ext = String_set.of_list t.ext;
              command =
                (fun ~flags ~sources:_ ~output ->
                  List.fold_left
                    (fun (acc : string list) x ->
                      if String.equal x "#output" then
                        acc @ [ Eio.Path.native_exn output.path ]
                      else if String.equal x "#flags" then acc @ flags.compile
                      else acc @ [ x ])
                    [] cmd);
            }
      | None -> (
          match find_compiler t.name with
          | None -> invalid_arg ("unknown compiler: " ^ t.name)
          | Some x -> x)

    let linker t =
      match t.command with
      | Some cmd ->
          Linker.
            {
              name = t.name;
              link_type = t.link_type;
              command =
                (fun ~flags ~objs ~output ->
                  List.fold_left
                    (fun (acc : string list) x ->
                      if String.equal x "#objs" then
                        let objs =
                          List.map
                            (fun obj ->
                              Eio.Path.native_exn obj.Object_file.path)
                            objs
                        in
                        acc @ objs
                      else if String.equal x "#output" then
                        acc @ [ Eio.Path.native_exn output ]
                      else if String.equal x "#flags" then acc @ flags.link
                      else acc @ [ x ])
                    [] cmd);
            }
      | None -> (
          match find_linker t.name with
          | None -> invalid_arg ("unknown linker: " ^ t.name)
          | Some x -> x)
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
        Compiler_config.clangxx;
        Compiler_config.ispc;
        Compiler_config.ghc;
      ]

    let default_linkers =
      [
        Compiler_config.clang;
        Compiler_config.clangxx;
        Compiler_config.{ clang with name = "ar" };
        Compiler_config.ghc;
      ]

    type t = {
      name : string option; [@default None]
      root : string option;
      target : string option; [@default None]
      compilers : Compiler_config.t list; [@default default_compilers]
      linker : Compiler_config.t; [@default Compiler_config.clang]
      files : string list; [@default []]
      ignore : string list; [@default []]
      flags : Lang_flags.t list; [@default []]
      script : string option; [@default None]
      after : string option; [@default None]
      disable_cache : bool; [@default false]
      only_if : string option; [@default None] [@key "if"]
      pkgconf : string list; [@default []] [@key "pkg"]
    }
    [@@deriving yaml]

    let default =
      {
        name = None;
        target = Some "a.out";
        root = Some ".";
        ignore = [];
        compilers = default_compilers;
        linker = Compiler_config.clang;
        files = [];
        script = None;
        after = None;
        flags = [];
        disable_cache = false;
        only_if = None;
        pkgconf = [];
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
                 ~default:(Option.value ~default:"default" config.root)
                 config.name)
          in
          None
        else
          let linker = Compiler_config.linker config.Build_config.linker in
          let compilers =
            List.map Compiler_config.compiler t.compilers
            @ List.map Compiler_config.compiler config.compilers
          in
          let compiler_flags =
            List.to_seq (t.flags @ config.flags)
            |> Seq.map (fun v ->
                   ( v.Build_config.Lang_flags.lang,
                     Flags.v ~compile:v.compile ~link:v.link () ))
            |> List.of_seq
          in
          let source =
            match config.root with
            | None -> path
            | Some p -> Eio.Path.(path / p)
          in
          let output =
            Option.map (fun output -> Eio.Path.(env#fs / output)) config.target
          in
          let name =
            match config.name with
            | Some name -> name
            | None -> (
                match config.target with Some p -> p | None -> "default")
          in
          let build =
            Build.v ?script:config.script ~pkgconf:config.pkgconf
              ?after:config.after ~linker ~compilers ~compiler_flags ?output
              ~source ~files:config.files ~name ~ignore:config.ignore env
          in
          Some build)
      t.build

  let load ~env path =
    let config = read_file_or_default path in
    match config with
    | Ok config -> Ok (init ~env path config)
    | Error e -> Error e
end
