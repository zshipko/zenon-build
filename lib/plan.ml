open Types
open Flags
open Source_file
open Linker
open Compiler
open Compiler_set
module Util = Util
module Build = Build
module Pkg_config = Pkg_config

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
      "compiler:" ^ c.name ^ "_" ^ String.concat "_" f.link
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

module Cycles = Graph.Cycles.Johnson (G)

type t = { graph : G.t }

let v () = { graph = G.create () }
let graph t = t.graph

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
  Eio.Fiber.List.iter
    (fun source_file ->
      if Re.execp ignore (Eio.Path.native_exn source_file.path) then ()
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
              match e with
              | Some _ -> e (* If there's a script, use that *)
              | None -> Some (Linker b.linker)
              (* Otherwise use the linker *)
            in
            G.add_edge_e t.graph @@ G.E.create obj_node edge_label node)
          output_node)
    source_files

let run_build t ?(execute = false) ?(execute_args = []) (b : Build.t) =
  Util.log "◎ BUILD %s" b.name;
  Option.iter
    (fun s ->
      Util.log "• SCRIPT %s" s;
      Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
    b.script;
  let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
  let flags = Flags.v ~compile:(Build.compile_flags b) () in
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
                let () =
                  let task =
                    Compiler.compile_obj c b.env#process_mgr ~sources
                      ~output:obj ~sw ~build_mtime:b.mtime
                      (Flags.concat b_flags flags)
                  in
                  let () = link_flags := Flags.concat !link_flags flags in
                  Option.iter Eio.Process.await_exn task
                in
                Some obj
            | _ -> None))
      objs
  in
  Option.iter
    (fun output ->
      if !count > 0 then (
        Util.log "⁕ LINK(%s) %s" b.linker.name (Eio.Path.native_exn output);
        Linker.link b.linker b.env#process_mgr ~objs ~output ~flags:!link_flags))
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
  let build_map =
    List.fold_left
      (fun acc (build : Build.t) ->
        if Hashtbl.mem acc build.name then
          Fmt.failwith "Duplicate build name: %s" build.name;
        Hashtbl.add acc build.name build;
        acc)
      (Hashtbl.create (List.length builds))
      builds
  in

  (* Check command availability before building *)
  if not (List.is_empty builds) then (
    let env = (List.hd builds).env in
    let checker = Command.v env#process_mgr in
    let required_commands = ref String_set.empty in
    List.iter
      (fun (build : Build.t) ->
        Hashtbl.iter
          (fun _ compiler ->
            let cmd = Compiler.get_command_name compiler in
            required_commands := String_set.add cmd !required_commands)
          build.compiler_index;
        let linker_cmd = Linker.get_command_name build.linker in
        required_commands := String_set.add linker_cmd !required_commands)
      builds;
    Command.check_commands checker (String_set.to_list !required_commands));

  (* Add build dependency edges to the existing graph *)
  Eio.Fiber.List.iter
    (fun build ->
      List.iter
        (fun dep_name ->
          match Hashtbl.find_opt build_map dep_name with
          | Some dep_build ->
              (* Edge from dependency to dependent: dep_build -> build *)
              G.add_edge t.graph (Build dep_build) (Build build)
          | None ->
              Fmt.failwith "Build '%s' depends on unknown build '%s'" build.name
                dep_name)
        build.depends_on)
    builds;

  let ndeps = Hashtbl.create (List.length builds) in
  List.iter
    (fun (build : Build.t) ->
      Hashtbl.add ndeps build.name (List.length build.depends_on))
    builds;

  (* Find all builds with no dependencies (in-degree 0) *)
  let ready =
    ref
      (Eio.Fiber.List.filter
         (fun (build : Build.t) -> Hashtbl.find ndeps build.name = 0)
         builds)
  in

  let executed = Hashtbl.create (List.length builds) in

  (* Build targets in parallel, level by level *)
  while not (List.is_empty !ready) do
    (* Build all ready targets in parallel *)
    let builds =
      Eio.Fiber.List.map
        (fun (build : Build.t) ->
          run_build ?execute ?execute_args:args t build;
          Hashtbl.add executed build.name ();
          build)
        !ready
    in

    (* Update in-degrees and find newly ready builds *)
    let count = ref 0 in
    ready :=
      List.fold_left
        (fun newly_ready (completed_build : Build.t) ->
          incr count;
          (* For each build that depends on this completed build *)
          G.fold_succ
            (fun succ acc ->
              match succ with
              | Build dependent ->
                  let n = Int.succ @@ Hashtbl.find ndeps dependent.name in
                  Hashtbl.replace ndeps dependent.name n;
                  if n = 0 && not (Hashtbl.mem executed dependent.name) then
                    dependent :: acc
                  else acc
              | _ -> acc)
            t.graph (Build completed_build) newly_ready)
        [] builds
  done
