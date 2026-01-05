open Types
open Flags
open Source_file
open Linker
open Compiler
module Util = Util
module Build = Build
module Pkg_config = Pkg_config

(* Exception for build failures that have already been logged *)
exception Build_failed of string

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
  | Dependency

let cmd_id = function
  | Script (b, output) ->
      "script:"
      ^ (Digest.string
           ((Option.map Eio.Path.native_exn output |> Option.value ~default:"")
           ^ b)
        |> Digest.to_hex)
  | Compiler (c, Some f) ->
      "compiler:" ^ c.name ^ "_" ^ String.concat "_" f.link
      ^ String.concat "_" f.compile
  | Compiler (c, None) -> "compiler:" ^ c.name
  | Linker link -> "linker:" ^ link.name
  | Dependency -> "dependency"

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

type t = { graph : G.t }

let v () = { graph = G.create () }
let graph t = t.graph

let build t (b : Build.t) =
  let () =
    if List.is_empty b.files && Option.is_none b.script then
      Build.add_source_file b "*.c"
  in
  let source_files = Build.locate_source_files b in
  let flags = Flags.v () in
  let build_node = Build b in
  let output_node = Option.map (fun x -> Output x) b.output in
  G.add_vertex t.graph build_node;
  Option.iter (G.add_vertex t.graph) output_node;
  let ignore_regex = Re.compile (Re.alt b.ignore) in
  let linker =
    Linker.auto_select_linker ~sources:source_files ~linker:b.linker ()
  in
  Eio.Fiber.List.iter
    (fun source_file ->
      if Re.execp ignore_regex (Eio.Path.native_exn source_file.path) then ()
      else
        let src_node = Src (source_file : Source_file.t) in
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
            (Object_file.of_source ~root:b.source ~build_name:b.name
               ~build_dir:Eio.Path.(b.build / "obj")
               source_file)
        in
        G.add_edge_e t.graph @@ G.E.create build_node e src_node;
        let ext = Source_file.ext (source_file : Source_file.t) in
        let compiler =
          match Hashtbl.find_opt b.compiler_index ext with
          | Some x -> x
          | None ->
              Fmt.failwith
                "failed to pick compiler for extension %s in compilers: %a  \n\
                \                 with extensions %a"
                ext
                (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string)
                (Compiler_set.to_list b.compilers |> List.map (fun c -> c.name))
                (Fmt.list
                   ~sep:(Fmt.const Fmt.string ", ")
                   (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string))
                (Compiler_set.to_list b.compilers
                |> List.map (fun c -> String_set.to_list c.ext))
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
          (fun node ->
            let edge_label =
              match e with Some _ -> e | None -> Some (Linker linker)
            in
            G.add_edge_e t.graph @@ G.E.create obj_node edge_label node)
          output_node)
    source_files

let run_script mgr ~build_dir s =
  let cmd = [ "sh"; "-c"; s ] in
  Log_file.with_log_file ~build_dir ~name:(Digest.to_hex (Digest.string s))
  @@ fun (tmp_path, log_file) ->
  try Eio.Process.run mgr cmd ~stdout:log_file ~stderr:log_file
  with exn ->
    (try
       let log = Eio.Path.load tmp_path in
       Util.log_clear "❌ script failed %s\n%s" s log
     with _ -> ());
    raise exn

let run_build t ?(execute = false) ?(execute_args = []) ?(log_level = `Quiet)
    (b : Build.t) =
  let verbose = Util.is_verbose log_level in
  Util.log "◎ %s" b.name;
  Option.iter
    (fun s ->
      Util.log ~verbose "• SCRIPT %s" s;
      run_script b.env#process_mgr ~build_dir:b.build s)
    b.script;
  let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
  let flags = Flags.v () in
  let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in
  let link_flags = ref b_flags in
  let count = ref 0 in
  let sources, objs =
    G.fold_succ_e
      (fun edge (sources, objs) ->
        match G.E.dst edge with
        | Src s as v' ->
            let obj =
              Object_file.of_source ~root:b.source ~build_name:b.name
                ~build_dir:Eio.Path.(b.build / "obj")
                s
            in
            (s :: sources, (v', obj) :: objs)
        | _ -> (sources, objs))
      t.graph (Build b) ([], [])
  in
  let linker = Linker.auto_select_linker ~sources ~linker:b.linker () in

  let visited_flags = Hashtbl.create 8 in

  let start = Unix.gettimeofday () in

  if not verbose then Util.init_progress (List.length objs);

  Eio.Switch.run @@ fun sw ->
  let objs =
    (if b.parallel then fun f x -> Eio.Fiber.List.filter_map f x
     else List.filter_map)
      (fun (v', obj) ->
        let obj_node = Obj obj in
        match G.find_edge t.graph v' obj_node |> G.E.label with
        | None -> None
        | Some edge' -> (
            match edge' with
            | Compiler (c, flags) ->
                incr count;
                let flags = Option.value ~default:(Flags.v ()) flags in
                let () =
                  let combined_flags = Flags.concat b_flags flags in
                  let key =
                    c.name
                    ^ String.concat "_" flags.link
                    ^ String.concat "_" flags.compile
                  in
                  try
                    let cmd =
                      c.command ~sources ~flags:combined_flags ~output:obj
                    in
                    if log_level = `Debug then
                      Util.log "  $ %s" (String.concat " " cmd);
                    let task =
                      Compiler.compile_obj c b.env#process_mgr ~sources
                        ~output:obj ~sw ~build_mtime:b.mtime ~verbose
                        ~build_dir:b.build combined_flags
                    in
                    (match task with
                    | Some (log_path, process) -> (
                        try
                          Eio.Process.await_exn process;
                          Eio.Path.unlink log_path (* Delete log on success *)
                        with exn ->
                          let log = Eio.Path.load log_path in
                          Eio.Path.unlink log_path;
                          let cmd =
                            c.command ~sources ~flags:combined_flags ~output:obj
                          in
                          let filepath =
                            Util.relative_to b.source obj.source.path
                          in
                          Util.log_error ~log_output:log ~filepath
                            ~target:b.name ~command:(String.concat " " cmd) ~exn
                            ();
                          raise (Build_failed b.name))
                    | None -> ());
                    if not (Hashtbl.mem visited_flags key) then
                      let () = Hashtbl.add visited_flags key true in
                      link_flags := Flags.concat !link_flags flags
                  with
                  | Build_failed _ as e -> raise e
                  | Eio.Cancel.Cancelled _ as e -> raise e
                  | exn ->
                      let cmd =
                        c.command ~sources ~flags:combined_flags ~output:obj
                      in
                      let filepath =
                        Util.relative_to b.source obj.source.path
                      in
                      Util.log_error ~log_output:"" ~filepath ~target:b.name
                        ~command:(String.concat " " cmd) ~exn ();
                      raise (Build_failed b.name)
                in
                Some obj
            | _ -> None))
      objs
  in
  Option.iter
    (fun output ->
      Util.log ~verbose "⁕ LINK(%s) %s" linker.name (Eio.Path.native_exn output);
      (if log_level = `Debug then
         let link_cmd = linker.command ~flags:!link_flags ~objs ~output in
         Util.log "  $ %s" (String.concat " " link_cmd));
      Linker.link linker b.env#process_mgr ~objs ~output ~flags:!link_flags
        ~build_dir:b.build)
    b.output;
  Option.iter
    (fun s ->
      Util.log ~verbose "• SCRIPT %s" s;
      run_script b.env#process_mgr ~build_dir:b.build s)
    b.after;
  (* Finalize progress bar for non-verbose mode *)
  if not verbose then Util.finalize_progress ();
  Util.log "✓ %s (%fsec) " b.name (Unix.gettimeofday () -. start);
  if execute then
    match b.output with
    | None -> Fmt.failwith "target %s has no output" b.name
    | Some exe ->
        Eio.Process.run b.env#process_mgr
          (Eio.Path.native_exn exe :: execute_args)

let build_map builds =
  let build_map = Hashtbl.create (List.length builds) in
  List.iter
    (fun (build : Build.t) ->
      if Hashtbl.mem build_map build.name then
        Fmt.failwith "duplicate build name: %s" build.name;
      Hashtbl.add build_map build.name build)
    builds;
  build_map

let add_dependency_edges t builds =
  let build_map = build_map builds in
  List.iter
    (fun build ->
      List.iter
        (fun dep_name ->
          match Hashtbl.find_opt build_map dep_name with
          | Some dep_build ->
              (* Edge from dependency to dependent: dep_build -> build *)
              G.add_edge_e t.graph
              @@ G.E.create (Build dep_build) (Some Dependency) (Build build)
          | None ->
              Fmt.failwith "build '%s' depends on unknown build '%s'" build.name
                dep_name)
        build.depends_on)
    builds

let run_all ?execute ?args ?(log_level = `Debug) t builds =
  add_dependency_edges t builds;

  (* Check command availability before building *)
  (if not (List.is_empty builds) then
     let env = (List.hd builds).env in
     let checker = Command.v env#process_mgr in
     let required_commands =
       List.fold_left
         (fun acc (build : Build.t) ->
           let acc =
             Hashtbl.fold
               (fun _ compiler acc ->
                 let cmd = compiler.Compiler.name in
                 String_set.add cmd acc)
               build.compiler_index acc
           in
           let linker_cmd = build.linker.name in
           String_set.add linker_cmd acc)
         String_set.empty builds
     in
     Command.check_commands checker (String_set.to_list required_commands));

  let ndeps = Hashtbl.create (List.length builds) in
  List.iter
    (fun (build : Build.t) ->
      Hashtbl.add ndeps build.name (List.length build.depends_on))
    builds;

  (* Find all builds with no dependencies *)
  let ready =
    ref
      (Eio.Fiber.List.filter
         (fun (build : Build.t) -> Hashtbl.find ndeps build.name = 0)
         builds)
  in

  let executed = Hashtbl.create (List.length builds) in

  (* Build targets in parallel, level by level *)
  while not (List.is_empty !ready) do
    let builds =
      Eio.Fiber.List.filter_map
        (fun (build : Build.t) ->
          try
            run_build ?execute ?execute_args:args ~log_level t build;
            Hashtbl.add executed build.name ();
            Some build
          with
          | Build_failed _ ->
              (* Error already logged *)
              Util.log_clear "❌ %s" build.name;
              None
          | Failure msg ->
              Util.log_clear "❌ %s\n%s" build.name msg;
              None)
        !ready
    in

    let count = ref 0 in
    ready :=
      List.fold_left
        (fun newly_ready (completed_build : Build.t) ->
          incr count;
          G.fold_succ
            (fun succ acc ->
              match succ with
              | Build dependent ->
                  let n = Int.pred @@ Hashtbl.find ndeps dependent.name in
                  Hashtbl.replace ndeps dependent.name n;
                  if n = 0 && not (Hashtbl.mem executed dependent.name) then
                    dependent :: acc
                  else acc
              | _ -> acc)
            t.graph (Build completed_build) newly_ready)
        [] builds
  done
