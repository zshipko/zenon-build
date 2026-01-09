open Types
open Flags
open Source_file
open Linker
open Compiler
module Util = Util
module Build = Build
module Pkg_config = Pkg_config

module Make = Graph.Imperative.Digraph.ConcreteLabeled (struct
  type t = Node.t

  let compare a b = String.compare (Node.node_id a) (Node.node_id b)
  let hash t = Hashtbl.hash (Node.node_id t)
  let equal a b = compare a b = 0
end)

module G = Make (struct
  type t = Node.edge option

  let default = None

  let compare a b =
    Option.compare String.compare
      (Option.map Node.edge_id a)
      (Option.map Node.edge_id b)
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
      Hashtbl.to_seq_keys b.compiler_index
      |> Seq.iter (fun ext -> Build.add_source_file b ("**." ^ ext))
  in
  let source_files = Build.locate_source_files b |> List.of_seq in
  Hashtbl.replace t.ordered_sources b.name source_files;
  let flags = Flags.v () in
  let build_node = Node.Build b in
  let output_node = Option.map (fun x -> Node.Output x) b.output in
  G.add_vertex t.graph build_node;
  Option.iter (G.add_vertex t.graph) output_node;
  let ignore_regex = Re.compile (Re.alt b.ignore) in
  let linker =
    Linker.auto_select_linker ~sources:source_files ?output:b.output
      ?linker:b.linker b.name
  in
  Eio.Fiber.List.iter
    (fun (source_file : Source_file.t) ->
      if Re.execp ignore_regex (Eio.Path.native_exn source_file.path) then ()
      else
        let src_node = Node.Src (source_file : Source_file.t) in
        G.add_vertex t.graph src_node;
        let e =
          match b.script with
          | Some s ->
              Some
                (Node.Script
                   (s, if List.is_empty source_files then b.output else None))
          | None -> None
        in
        let obj_node =
          Node.Obj
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
          let lang_flags =
            Hashtbl.find_opt b.compiler_flags ext
            |> Option.value ~default:b.flags
          in
          let flags_with_c =
            if Util.extension_is_c_or_cxx ext then lang_flags
            else
              let c_flags =
                Hashtbl.find_opt b.compiler_flags "c"
                |> Option.value ~default:(Flags.v ())
              in
              Flags.concat (compiler.wrap_c_flags c_flags) lang_flags
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

let run_build t ?(execute = false) ?(execute_args = []) (b : Build.t) =
  let verbose = Util.is_verbose b.log_level in
  Util.log_clear "◎ %s" b.name;
  Option.iter Eff.script b.script;
  let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
  (* need to preserve order of files from config *)
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
        (Node.Src s, obj))
      sources
  in
  let linker =
    Linker.auto_select_linker ~sources ?output:b.output ?linker:b.linker b.name
  in
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
  (* combine the compiler-specifiv flags with the wrapped c flags *)
  let b_flags =
    let pkg_flags = Flags.v ~compile:pkg.compile ~link:pkg.link () in
    let wrapped_pkg_flags = primary_compiler.wrap_c_flags pkg_flags in
    Flags.concat wrapped_pkg_flags b.flags
  in
  let link_flags = ref b_flags in
  let start = Unix.gettimeofday () in
  let visited_flags = Hashtbl.create 8 in
  let visited_objects = Queue.create () in
  let build_state_mutex = Eio.Mutex.create () in

  (* Group objects by compiler for better parallelism *)
  let objs_by_compiler = Hashtbl.create 8 in
  List.iter
    (fun (v', obj) ->
      let obj_node = Node.Obj obj in
      match G.E.label @@ G.find_edge t.graph v' obj_node with
      | None -> ()
      | Some edge' -> (
          match edge' with
          | Compiler (c, flags) ->
              let obj = c.transform_output obj in
              let entry = (c, flags, obj) in
              let entries = Hashtbl.find_opt objs_by_compiler c.name in
              Hashtbl.replace objs_by_compiler c.name
                (entry :: Option.value ~default:[] entries)
          | _ -> ()))
    objs;

  (* Sort compilers: parallel ones first, then sequential ones *)
  let par, seq =
    Hashtbl.fold
      (fun name entries acc -> (name, List.rev entries) :: acc)
      objs_by_compiler []
    |> List.sort (fun (_, entries1) (_, entries2) ->
        match (entries1, entries2) with
        | (c1, _, _) :: _, (c2, _, _) :: _ -> (
            match (c1.Compiler.parallel, c2.Compiler.parallel) with
            | true, false -> -1 (* parallel first *)
            | false, true -> 1
            | _ -> String.compare c1.name c2.name)
        | _ -> assert false)
    |> List.partition (fun (_, entries) ->
        match entries with (c, _, _) :: _ -> c.Compiler.parallel | [] -> false)
  in

  let update_build_state
      (objects_info : (Object_file.t * string * Flags.t option) list) =
    Eio.Mutex.use_rw ~protect:false build_state_mutex (fun () ->
        List.iter
          (fun (obj, compiler_name, flags_opt) ->
            let flags = Option.value ~default:(Flags.v ()) flags_opt in
            Queue.push obj visited_objects;
            let key =
              Digest.to_hex @@ Digest.string @@ compiler_name
              ^ String.concat "_" flags.link
              ^ String.concat "_" flags.compile
            in
            if not (Hashtbl.mem visited_flags key) then (
              Hashtbl.add visited_flags key true;
              link_flags := Flags.concat !link_flags flags))
          objects_info)
  in

  (* Handle parallel compiler groups *)
  let parallel_tasks =
    let objects_for_compilation = Queue.to_seq visited_objects in
    List.concat_map
      (fun (_, entries) ->
        let to_compile, cached =
          List.partition_map
            (fun (c, flags, obj) ->
              let flags' = Option.value ~default:(Flags.v ()) flags in
              let combined_flags = Flags.concat b_flags flags' in
              match
                Eff.compile ~compiler:c ~output:obj
                  ~objects:objects_for_compilation ~flags:combined_flags
              with
              | Some task -> Left (c, obj, flags, combined_flags, task)
              | None -> Right (obj, c.name, flags))
            entries
        in
        (* cached object files *)
        update_build_state cached;
        to_compile)
      par
  in

  let compiled_objs =
    let objects_for_linking = Queue.to_seq visited_objects in
    Eio.Fiber.List.filter_map
      (fun (c, obj, flags, combined_flags, proc) ->
        try
          Eff.wait_process proc;
          Some (obj, c.name, flags)
        with exn ->
          let cmd =
            c.Compiler.command ~flags:combined_flags ~output:obj
              ~objects:objects_for_linking
          in
          Eff.build_error b obj ~cmd ~exn proc)
      parallel_tasks
  in
  update_build_state compiled_objs;

  (* sequential compilers *)
  List.iter
    (fun (_, entries) ->
      if not (List.is_empty entries) then
        List.iter
          (fun (c, flags, obj) ->
            let combined_flags =
              let flags' = Option.value ~default:(Flags.v ()) flags in
              Flags.concat b_flags flags'
            in
            try
              let objects_for_compilation = Queue.to_seq visited_objects in
              let task =
                Eff.compile ~compiler:c ~output:obj
                  ~objects:objects_for_compilation ~flags:combined_flags
              in
              match task with
              | Some process -> (
                  try
                    Eff.wait_process process;
                    update_build_state [ (obj, c.name, flags) ]
                  with exn ->
                    let cmd =
                      c.Compiler.command ~flags:combined_flags ~output:obj
                        ~objects:objects_for_compilation
                    in
                    Eff.build_error b obj ~cmd ~exn process)
              | None -> update_build_state [ (obj, c.name, flags) ]
            with
            | Eff.Build_failed _ as e -> raise e
            | Eio.Cancel.Cancelled _ as e -> raise e
            | exn ->
                let cmd =
                  c.Compiler.command ~flags:combined_flags ~output:obj
                    ~objects:(Queue.to_seq visited_objects)
                in
                let filepath = Util.relative_to b.source obj.source.path in
                Util.log_error ~log_output:"" ~filepath ~target:b.name
                  ~command:(String.concat " " cmd) ~exn ();
                raise (Eff.Build_failed b.name))
          entries)
    seq;

  let objs = Queue.to_seq visited_objects |> List.of_seq in
  Option.iter
    (fun output ->
      Util.log ~verbose "⁕ LINK(%s) %s" linker.name (Eio.Path.native_exn output);
      let final_link_flags =
        {
          !link_flags with
          link = Util.remove_duplicates_preserving_order !link_flags.link;
        }
      in
      Eff.link ~output ~linker ~objects:objs ~flags:final_link_flags)
    b.output;
  Option.iter Eff.script b.after;
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
              let ext_dep =
                Node.External { path; target = target_name; build }
              in
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
            | Node.Build b -> Some b.Build.name
            | Node.External e -> Some (e.path ^ ":" ^ e.target)
            | _ -> None)
          cycle
      in
      if not (List.is_empty cycle_names) then
        Fmt.failwith "Circular dependency detected: %s"
          (String.concat " -> " cycle_names))
    t.graph

let run_all ?execute ?args ~log_level t ~env builds =
  add_dependency_edges t builds;
  check_dependency_cycles t;

  (* Check command availability before building *)
  let checker = Command.v env#process_mgr in
  let required_commands =
    List.fold_left
      (fun acc (build : Build.t) ->
        Hashtbl.fold
          (fun _ compiler acc ->
            let cmd = compiler.Compiler.name in
            String_set.add cmd acc)
          build.compiler_index acc)
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
      Hashtbl.add ndeps
        (Node.node_id (Build build))
        (List.length build.depends_on))
    builds;
  List.iter
    (fun (e : Build.External.t) ->
      let key = Node.node_id (External e) in
      let n = G.in_degree t.graph (External e) in
      Hashtbl.add ndeps key n)
    external_deps;

  let ready = ref [] in
  List.iter
    (fun (build : Build.t) ->
      if Hashtbl.find ndeps (Node.node_id (Build build)) = 0 then
        ready := Node.Build build :: !ready)
    builds;

  List.iter
    (fun (e : Build.External.t) ->
      if Hashtbl.find ndeps (Node.node_id (External e)) = 0 then
        ready := External e :: !ready)
    external_deps;

  let visited =
    Hashtbl.create (List.length builds + List.length external_deps)
  in

  Eio.Switch.run @@ fun sw ->
  while not (List.is_empty !ready) do
    let completed =
      Eio.Fiber.List.filter_map
        (fun node ->
          try
            match node with
            | Node.Build build ->
                Eff.handle build ~visited ~sw @@ fun () ->
                let () = run_build ?execute ?execute_args:args t build in
                Hashtbl.add visited (Node.node_id node) ();
                Some node
            | External ext ->
                Eff.handle ext.build ~visited ~sw @@ fun () ->
                Eff.extern ext;
                Some node
            | _ -> None
          with
          | Eff.Build_failed _ ->
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
              let succ_id = Node.node_id succ in
              match Hashtbl.find_opt ndeps succ_id with
              | Some n_deps ->
                  let n = Int.pred n_deps in
                  Hashtbl.replace ndeps succ_id n;
                  if n = 0 && not (Hashtbl.mem visited succ_id) then succ :: acc
                  else acc
              | None -> acc)
            t.graph completed_node newly_ready)
        [] completed
  done;

  (* Finalize progress bar after all builds complete *)
  if not verbose then Util.finalize_progress ()
