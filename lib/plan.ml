open Types
open Flags
open Source_file
open Linker
open Compiler
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

let run_build t ?(execute = false) ?(execute_args = []) ?(verbose = false)
    ?(shared_progress = None) (b : Build.t) =
  Util.log "◎ %s" b.name;
  Option.iter
    (fun s ->
      if verbose then Util.log "• SCRIPT %s" s;
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
  (* Auto-select linker based on source file extensions if no explicit linker was set *)
  let linker =
    match b.Build.linker.name with
    | "clang" | "gcc" -> (
        (* Only auto-select if using generic C linker *)
        let source_exts =
          List.fold_left
            (fun acc src -> String_set.add (Source_file.ext src) acc)
            String_set.empty sources
        in
        (* Use all available linkers for auto-selection, including custom ones *)
        let available_linkers = !Linker.all in
        match Linker.auto_select_linker ~source_exts available_linkers with
        | Ok (Some linker) -> linker
        | Ok None -> b.linker (* No specialized linker needed *)
        | Error msg -> Fmt.failwith "%s" msg)
    | _ -> b.linker
  in
  let total_files = List.length objs in
  let start_time = Unix.gettimeofday () in

  (* Spinner frames using Unicode characters *)
  let spinner = [| "◜"; "◝"; "◞"; "◟" |] in

  (* Truncate filename for display *)
  let truncate_path path max_len =
    if String.length path <= max_len then path
    else
      let len = String.length path in
      "..." ^ String.sub path (len - max_len + 3) (max_len - 3)
  in

  (* Create an animated progress bar *)
  let report_progress current_file =
    match shared_progress with
    | Some
        (total_files_all, completed_all, start_time_all, spinner_idx_all, mutex)
      ->
        (* Update shared progress bar *)
        let n = Atomic.fetch_and_add completed_all 1 + 1 in
        Eio.Mutex.use_rw mutex ~protect:false (fun () ->
            let elapsed = Unix.gettimeofday () -. start_time_all in
            let percent =
              float_of_int n /. float_of_int total_files_all *. 100.0
            in
            let bar_width = 20 in
            let filled =
              int_of_float (float_of_int bar_width *. percent /. 100.0)
            in
            let bar =
              let buf = Buffer.create (bar_width * 3) in
              for i = 0 to bar_width - 1 do
                Buffer.add_string buf (if i < filled then "█" else "░")
              done;
              Buffer.contents buf
            in
            let idx = Atomic.fetch_and_add spinner_idx_all 1 in
            let file_display = truncate_path current_file 40 in
            Fmt.epr "\r%s [%s] %.0f%% (%d/%d) %.1fs • %s%!"
              spinner.(idx mod Array.length spinner)
              bar percent n total_files_all elapsed file_display)
    | None -> ()
  in

  let visited_flags = Hashtbl.create 8 in
  let objs =
    Eio.Switch.run @@ fun sw ->
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
                  let filepath = Util.relative_to b.source obj.source.path in
                  let combined_flags = Flags.concat b_flags flags in
                  let key =
                    c.name
                    ^ String.concat "_" flags.link
                    ^ String.concat "_" flags.compile
                  in
                  (try
                     let task, _log_path_opt =
                       Compiler.compile_obj c b.env#process_mgr ~sources
                         ~output:obj ~sw ~build_mtime:b.mtime ~verbose
                         ~fs:b.env#fs combined_flags
                     in
                     (match task with
                     | Some (process, log_path) -> (
                         try
                           Eio.Process.await_exn process;
                           (* Delete log file on success *)
                           try Eio.Path.unlink log_path with _ -> ()
                         with exn ->
                           (* On error, show the log file contents *)
                           let log_contents =
                             Eio.Path.load log_path |> String.trim
                           in
                           if String.length log_contents > 0 then
                             Fmt.epr "\n%s\n%!" log_contents;
                           raise exn)
                     | None -> ());
                     if not (Hashtbl.mem visited_flags key) then
                       let () = Hashtbl.add visited_flags key true in
                       link_flags := Flags.concat !link_flags flags
                   with exn ->
                     let cmd =
                       c.command ~sources ~flags:combined_flags ~output:obj
                     in
                     Fmt.failwith
                       "compilation failed for '%s' in target '%s'\n\
                        \tcommand: %s\n\
                        \tmessage: %a"
                       filepath b.name (String.concat " " cmd) Fmt.exn exn);
                  report_progress filepath
                in
                Some obj
            | _ -> None))
      objs
  in
  (* Show completion *)
  let elapsed = Unix.gettimeofday () -. start_time in
  if total_files > 0 then
    match shared_progress with
    | Some (_, _, _, _, mutex) ->
        (* Use mutex to ensure completion message doesn't interleave with progress *)
        Eio.Mutex.use_rw mutex ~protect:false (fun () ->
            Fmt.epr "\r\027[K%!";
            Util.log "✓ %s • %d files in %.1fs" b.name total_files elapsed)
    | None -> (
        Util.log "✓ %s • %d files in %.1fs" b.name total_files elapsed;

        Option.iter
          (fun output ->
            if verbose then
              Util.log "⁕ LINK(%s) %s" linker.name (Eio.Path.native_exn output);
            Eio.Switch.run @@ fun sw ->
            let log_path =
              Linker.link linker b.env#process_mgr ~sw ~fs:b.env#fs ~objs
                ~output ~flags:!link_flags
            in
            (* Delete log file on success *)
            try Eio.Path.unlink log_path with _ -> ())
          b.output;
        Option.iter
          (fun s ->
            if verbose then Util.log "• SCRIPT %s" s;
            Eio.Process.run b.env#process_mgr [ "sh"; "-c"; s ])
          b.after;
        if execute then
          match b.output with
          | None -> Fmt.failwith "target %s has no output" b.name
          | Some exe ->
              Eio.Process.run b.env#process_mgr
                (Eio.Path.native_exn exe :: execute_args))

let run_all ?execute ?args ?(verbose = false) t builds =
  let build_map =
    List.fold_left
      (fun acc (build : Build.t) ->
        if Hashtbl.mem acc build.name then
          Fmt.failwith "duplicate build name: %s" build.name;
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
            let cmd = compiler.Compiler.name in
            required_commands := String_set.add cmd !required_commands)
          build.compiler_index;
        let linker_cmd = build.linker.name in
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
              Fmt.failwith "build '%s' depends on unknown build '%s'" build.name
                dep_name)
        build.depends_on)
    builds;

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
    (* Build all ready targets in parallel *)
    (* Calculate total files for shared progress bar *)
    let total_files =
      List.fold_left
        (fun acc (build : Build.t) ->
          let sources, _ =
            G.fold_succ_e
              (fun edge (sources, objs) ->
                match G.E.dst edge with
                | Src s as v' ->
                    let obj =
                      Object_file.of_source ~root:build.source
                        ~build_name:build.name
                        ~build_dir:Eio.Path.(build.build / "obj")
                        s
                    in
                    (s :: sources, (v', obj) :: objs)
                | _ -> (sources, objs))
              t.graph (Build build) ([], [])
          in
          acc + List.length sources)
        0 !ready
    in

    (* Create shared progress bar if we have enough files *)
    let shared_progress =
      if total_files > 3 && not verbose then
        Some
          ( total_files,
            Atomic.make 0,
            Unix.gettimeofday (),
            Atomic.make 0,
            Eio.Mutex.create () )
      else None
    in

    let builds =
      Eio.Fiber.List.filter_map
        (fun (build : Build.t) ->
          try
            run_build ?execute ?execute_args:args ~verbose ~shared_progress t
              build;
            Hashtbl.add executed build.name ();
            Some build
          with Failure msg ->
            Util.log "❌ %s\n%s" build.name msg;
            None)
        !ready
    in

    (* Clear shared progress bar *)
    (match shared_progress with
    | Some (_, _, _, _, mutex) ->
        Eio.Mutex.use_rw mutex ~protect:false (fun () -> Fmt.epr "\r\027[K%!")
    | None -> ());

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
                  let n = Int.pred @@ Hashtbl.find ndeps dependent.name in
                  Hashtbl.replace ndeps dependent.name n;
                  if n = 0 && not (Hashtbl.mem executed dependent.name) then
                    dependent :: acc
                  else acc
              | _ -> acc)
            t.graph (Build completed_build) newly_ready)
        [] builds
  done
