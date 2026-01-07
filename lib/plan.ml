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

type external_dep = { path : string; target : string; build : Build.t }

type value =
  | Build of Build.t
  | Src of Source_file.t
  | Obj of Object_file.t
  | Output of path
  | External of external_dep

let value_id = function
  | Build b -> "build:" ^ b.name
  | Src s -> "src:" ^ Eio.Path.native_exn s.path
  | Obj s -> "obj:" ^ Eio.Path.native_exn s.path
  | Output s -> "out:" ^ Eio.Path.native_exn s
  | External e -> "external:" ^ e.path ^ ":" ^ e.target

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

module Cycles = Graph.Cycles.Johnson (G)

type t = {
  graph : G.t;
  ordered_sources : (string, Source_file.t list) Hashtbl.t;
}

let v () = { graph = G.create (); ordered_sources = Hashtbl.create 16 }
let graph t = t.graph

let build t (b : Build.t) =
  let () =
    if List.is_empty b.files && Option.is_none b.script then
      Build.add_source_file b "*.c"
  in
  let source_files = Build.locate_source_files b in
  (* Store ordered source files in the plan for later use *)
  Hashtbl.replace t.ordered_sources b.name source_files;
  let flags = Flags.v () in
  let build_node = Build b in
  let output_node = Option.map (fun x -> Output x) b.output in
  G.add_vertex t.graph build_node;
  Option.iter (G.add_vertex t.graph) output_node;
  let ignore_regex = Re.compile (Re.alt b.ignore) in
  let linker =
    Linker.auto_select_linker ~sources:source_files ~linker:b.linker b.name
  in
  Eio.Fiber.List.iter
    (fun (source_file : Source_file.t) ->
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
                (Compiler.Set.to_list b.compilers |> List.map (fun c -> c.name))
                (Fmt.list
                   ~sep:(Fmt.const Fmt.string ", ")
                   (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string))
                (Compiler.Set.to_list b.compilers
                |> List.map (fun c -> String_set.to_list c.ext))
        in
        let flags =
          (* Only include compiler-specific flags in the edge, not b.flags
             since b.flags gets added in run_build *)
          let lang_flags =
            Hashtbl.find_opt b.compiler_flags ext
            |> Option.value ~default:(Flags.v ())
          in
          let flags_with_c =
            if Util.extension_is_c_or_cxx ext then lang_flags
            else
              let c_flags =
                Hashtbl.find_opt b.compiler_flags "c"
                |> Option.value ~default:(Flags.v ())
              in
              let wrapped_c_flags = compiler.wrap_c_flags c_flags in
              Flags.concat wrapped_c_flags lang_flags
          in
          Flags.concat flags flags_with_c
        in
        G.add_edge_e t.graph
        @@ G.E.create src_node (Some (Compiler (compiler, Some flags))) obj_node;
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
       let log = Log_file.get tmp_path in
       Util.log_clear "❌ script failed %s\n%s" s log
     with _ -> ());
    raise exn

let run_build t ?(execute = false) ?(execute_args = []) ?(log_level = `Quiet)
    (b : Build.t) =
  let verbose = Util.is_verbose log_level in
  Util.log_clear "◎ %s" b.name;
  Option.iter
    (fun s ->
      Util.log ~verbose "• SCRIPT %s" s;
      run_script b.env#process_mgr ~build_dir:b.build s)
    b.script;
  let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
  let flags = Flags.v () in
  (* Use ordered sources from Plan hashtable to preserve file order from config *)
  let sources =
    Hashtbl.find_opt t.ordered_sources b.name |> Option.value ~default:[]
  in
  let objs =
    List.map
      (fun s ->
        let obj =
          Object_file.of_source ~root:b.source ~build_name:b.name
            ~build_dir:Eio.Path.(b.build / "obj")
            s
        in
        (Src s, obj))
      sources
  in
  let linker = Linker.auto_select_linker ~sources ~linker:b.linker b.name in
  let primary_compiler =
    let source_exts =
      List.fold_left
        (fun acc src -> String_set.add (Source_file.ext src) acc)
        String_set.empty sources
    in
    Compiler.Set.to_list b.compilers
    |> List.find_opt (fun c ->
           not (String_set.is_empty (String_set.inter c.ext source_exts)))
    |> Option.value ~default:Compiler.clang
  in
  (* Wrap C flags for non-C builds (pkg-config and C-specific flags need wrapping) *)
  let b_flags =
    let c_flags =
      Hashtbl.find_opt b.compiler_flags "c"
      |> Option.value ~default:(Flags.v ())
    in
    let wrapped_c = primary_compiler.wrap_c_flags (Flags.concat pkg c_flags) in
    Flags.concat flags @@ Flags.concat wrapped_c b.flags
  in
  let link_flags = ref b_flags in
  let count = ref 0 in

  let start = Unix.gettimeofday () in
  let visited_flags = Hashtbl.create 8 in
  let visited_objects = ref [] in
  let do_parallel = ref b.parallel in

  Eio.Switch.run @@ fun sw ->
  let objs =
    List.map
      (fun (v', obj) ->
        let obj_node = Obj obj in
        match G.find_edge t.graph v' obj_node |> G.E.label with
        | None -> None
        | Some edge' -> (
            match edge' with
            | Compiler (c, flags) ->
                incr count;
                do_parallel := !do_parallel && c.parallel;
                let obj = c.transform_output obj in
                Option.some
                @@ ( obj,
                     fun () ->
                       let flags = Option.value ~default:(Flags.v ()) flags in
                       let () =
                         let combined_flags = Flags.concat b_flags flags in
                         let key =
                           Digest.to_hex @@ Digest.string @@ c.name
                           ^ String.concat "_" flags.link
                           ^ String.concat "_" flags.compile
                         in
                         try
                           let task =
                             Compiler.compile_obj c ~output:obj ~sw ~log_level
                               ~build_dir:b.build ~build_mtime:b.mtime
                               ~env:b.env ~objects:!visited_objects
                               combined_flags
                           in
                           (match task with
                           | Some (log_path, process) -> (
                               try
                                 Eio.Process.await_exn process;
                                 visited_objects := !visited_objects @ [ obj ];
                                 Eio.Path.unlink
                                   log_path (* Delete log on success *)
                               with exn ->
                                 let log = Log_file.get ~unlink:true log_path in
                                 let cmd =
                                   c.command ~flags:combined_flags ~output:obj
                                     ~objects:!visited_objects
                                 in
                                 let filepath =
                                   Util.relative_to b.source obj.source.path
                                 in
                                 Util.log_error ~log_output:log ~filepath
                                   ~target:b.name
                                   ~command:(String.concat " " cmd) ~exn ();
                                 raise (Build_failed b.name))
                           | None ->
                               visited_objects := !visited_objects @ [ obj ]);
                           if not (Hashtbl.mem visited_flags key) then
                             let () = Hashtbl.add visited_flags key true in
                             link_flags := Flags.concat !link_flags flags
                         with
                         | Build_failed _ as e -> raise e
                         | Eio.Cancel.Cancelled _ as e -> raise e
                         | exn ->
                             let cmd =
                               c.command ~flags:combined_flags ~output:obj
                                 ~objects:!visited_objects
                             in
                             let filepath =
                               Util.relative_to b.source obj.source.path
                             in
                             Util.log_error ~log_output:"" ~filepath
                               ~target:b.name ~command:(String.concat " " cmd)
                               ~exn ();
                             raise (Build_failed b.name)
                       in
                       () )
            | _ -> None))
      objs
  in
  let objs, tasks = List.filter_map Fun.id objs |> List.split in
  let () =
    if List.is_empty tasks then ()
    else if !do_parallel then Eio.Fiber.all tasks
    else List.iter (fun f -> f ()) tasks
  in
  Option.iter
    (fun output ->
      Util.log ~verbose "⁕ LINK(%s) %s" linker.name (Eio.Path.native_exn output);
      (if Util.is_debug log_level then
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
  Util.log_clear "✓ %s (%fsec) " b.name (Unix.gettimeofday () -. start);
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
          match String.split_on_char ':' dep_name with
          | [ path; target_name ] ->
              let ext_dep = External { path; target = target_name; build } in
              G.add_vertex t.graph ext_dep;
              G.add_edge_e t.graph
              @@ G.E.create ext_dep (Some Dependency) (Build build)
          | _ -> (
              (* Local dependency *)
              match Hashtbl.find_opt build_map dep_name with
              | Some dep_build ->
                  G.add_edge_e t.graph
                  @@ G.E.create (Build dep_build) (Some Dependency)
                       (Build build)
              | None ->
                  Fmt.failwith "build '%s' depends on unknown build '%s'"
                    build.name dep_name))
        build.depends_on)
    builds

let check_dependency_cycles t =
  Cycles.iter_cycles
    (fun cycle ->
      let cycle_names =
        List.filter_map
          (function
            | Build b -> Some b.Build.name
            | External e -> Some (e.path ^ ":" ^ e.target)
            | _ -> None)
          cycle
      in
      if not (List.is_empty cycle_names) then
        Fmt.failwith "Circular dependency detected: %s"
          (String.concat " -> " cycle_names))
    t.graph

let run_all ?execute ?args ?(log_level = `Debug) t ~env builds =
  add_dependency_edges t builds;
  check_dependency_cycles t;

  (* Check command availability before building *)
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
  Command.check_commands checker (String_set.to_list required_commands);

  let verbose = Util.is_verbose log_level in
  (if not verbose then
     let total_objs =
       List.fold_left
         (fun acc (build : Build.t) ->
           let sources =
             Hashtbl.find_opt t.ordered_sources build.name
             |> Option.value ~default:[]
           in
           acc + List.length sources)
         0 builds
     in
     Util.init_progress total_objs);

  let external_deps =
    G.fold_vertex
      (fun v acc -> match v with External e -> e :: acc | _ -> acc)
      t.graph []
  in

  let ndeps = Hashtbl.create (List.length builds + List.length external_deps) in
  List.iter
    (fun (build : Build.t) ->
      Hashtbl.add ndeps (value_id (Build build)) (List.length build.depends_on))
    builds;
  List.iter
    (fun (e : external_dep) ->
      let key = value_id (External e) in
      let n = G.in_degree t.graph (External e) in
      Hashtbl.add ndeps key n)
    external_deps;

  let ready = ref [] in
  List.iter
    (fun (build : Build.t) ->
      if Hashtbl.find ndeps (value_id (Build build)) = 0 then
        ready := Build build :: !ready)
    builds;

  List.iter
    (fun (e : external_dep) ->
      if Hashtbl.find ndeps (value_id (External e)) = 0 then
        ready := External e :: !ready)
    external_deps;

  let executed =
    Hashtbl.create (List.length builds + List.length external_deps)
  in

  while not (List.is_empty !ready) do
    let completed =
      Eio.Fiber.List.filter_map
        (fun node ->
          try
            match node with
            | Build build ->
                run_build ?execute ?execute_args:args ~log_level t build;
                Hashtbl.add executed (value_id node) ();
                Some node
            | External e ->
                let zenon_bin = Sys.executable_name in
                let start = Unix.gettimeofday () in
                Util.log_clear "◎ %s:%s" e.path e.target;
                let name =
                  Digest.to_hex @@ Digest.string
                  @@ Format.sprintf "%s:%s" e.path e.target
                in
                Log_file.with_log_file ~build_dir:e.build.build ~name
                @@ fun (tmp_file, log_file) ->
                (try
                   Eio.Process.run env#process_mgr ~stderr:log_file
                     ~stdout:log_file
                     ~cwd:Eio.Path.(env#fs / e.path)
                     [ zenon_bin; "build"; e.target ];
                   Util.log_clear "✓ %s:%s (%fsec)" e.path e.target
                     (Unix.gettimeofday () -. start);
                   Hashtbl.add executed (value_id node) ()
                 with _exn ->
                   let log = Log_file.get tmp_file in
                   Util.log_clear "%s" log;
                   raise (Build_failed ""));
                Some node
            | _ -> None
          with
          | Build_failed _ ->
              (* Error already logged *)
              (match node with
              | Build b -> Util.log_clear "❌ %s" b.name
              | External e -> Util.log_clear "❌ %s:%s" e.path e.target
              | _ -> assert false);
              None
          | Failure msg ->
              (match node with
              | Build b -> Util.log_clear "❌ %s\n%s" b.name msg
              | External e -> Util.log_clear "❌ %s:%s\n%s" e.path e.target msg
              | _ -> assert false);
              None)
        !ready
    in

    ready :=
      List.fold_left
        (fun newly_ready completed_node ->
          G.fold_succ
            (fun succ acc ->
              let succ_id = value_id succ in
              match Hashtbl.find_opt ndeps succ_id with
              | Some n_deps ->
                  let n = Int.pred n_deps in
                  Hashtbl.replace ndeps succ_id n;
                  if n = 0 && not (Hashtbl.mem executed succ_id) then
                    succ :: acc
                  else acc
              | None -> acc)
            t.graph completed_node newly_ready)
        [] completed
  done;

  (* Finalize progress bar after all builds complete *)
  if not verbose then Util.finalize_progress ()
