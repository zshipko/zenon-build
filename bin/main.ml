open Cmdliner
open Cmdliner.Term.Syntax
open Zenon

let path =
  let doc = "Root directory" in
  Arg.(
    value & opt string (Sys.getcwd ()) & info [ "p"; "path" ] ~doc ~docv:"PATH")

let output =
  let doc = "Output file name" in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv:"PATH")

let targets =
  let doc = "Selected targets" in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"TARGETS")

let cflag =
  let doc = "Compiler flag" in
  Arg.(value & opt_all string [] & info [ "cflag"; "c" ] ~doc ~docv:"FLAG")

let ldflag =
  let doc = "Linker flag" in
  Arg.(value & opt_all string [] & info [ "lflag"; "l" ] ~doc ~docv:"FLAG")

let ignore =
  let doc = "Ignore file" in
  Arg.(value & opt_all string [] & info [ "ignore"; "i" ] ~doc ~docv:"FILE")

let file =
  let doc = "Add file" in
  Arg.(value & opt_all string [] & info [ "file"; "f" ] ~doc ~docv:"FILE")

let run =
  let doc = "Run after compiling" in
  Arg.(value & flag & info [ "run"; "r" ] ~doc)

let arg =
  let doc = "Run argument" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc ~docv:"ARGUMENT")

let pkg =
  let doc = "Pkg-config package" in
  Arg.(value & opt_all string [] & info [ "pkg" ] ~doc ~docv:"PACKAGE")

let linker =
  let doc = "Linker name" in
  Arg.(value & opt (some string) None & info [ "linker" ] ~doc ~docv:"LINKER")

let verbose =
  let doc = "Control log verbosity (-v for verbose, -vv for debug logging)" in
  Arg.(value & flag_all & info [ "verbose"; "v" ] ~doc)

let rec collect_dependencies build_map name visited =
  if String_set.mem name visited then visited
  else
    let visited = String_set.add name visited in
    match Hashtbl.find_opt build_map name with
    | None -> visited
    | Some build ->
        List.fold_left
          (fun acc dep -> collect_dependencies build_map dep acc)
          visited build.Build.depends_on

let load_config env path =
  match Config.load ~env Eio.Path.(env#fs / path) with
  | Ok x -> x
  | Error (`Msg err) -> failwith err

let find_build builds name =
  match name with
  | Some name -> List.find_opt (fun b -> b.Build.name = name) builds
  | None -> ( try Some (List.hd builds) with _ -> None)

let lib_name b =
  let name = b.Build.name in
  let name =
    if String.starts_with ~prefix:"lib" name then
      String.sub name 3 (String.length name - 3)
    else name
  in
  Filename.remove_extension name

let c_flags b =
  Hashtbl.find_opt b.Build.compiler_flags "c"
  |> Option.value ~default:(Flags.v ())

let filter_builds builds names =
  if List.is_empty names then
    List.filter_map
      (fun x -> if x.Build.hidden then None else Some x.Build.name)
      builds
  else names

let make_build_map builds =
  List.fold_left
    (fun acc b ->
      Hashtbl.add acc b.Build.name b;
      acc)
    (Hashtbl.create (List.length builds))
    builds

let builds_with_deps build_map names =
  List.fold_left
    (fun acc name -> collect_dependencies build_map name acc)
    String_set.empty names

let build ?output ?(ignore = []) ~arg ~cflags ~ldflags ~path ~builds ~file ~run
    ~pkg ~(linker : string option) ~log_level () =
  Eio_posix.run @@ fun env ->
  let ignore_patterns = List.map Util.glob_path ignore in
  let x = load_config env path in
  let builds, x =
    match x with
    | [] ->
        ( [ "default" ],
          [
            Build.v env ~ignore:ignore_patterns ~pkgconf:pkg
              ~flags:(Flags.v ~compile:cflags ~link:ldflags ())
              ~source:Eio.Path.(env#fs / path)
              ~files:file ~name:"default"
              ?output:(Option.map (fun x -> Eio.Path.(env#cwd / x)) output)
              ?linker:
                (Option.map
                   (fun name ->
                     Config.Compiler_config.(
                       linker
                         { name; ext = []; command = None; link_type = "exe" }))
                   linker);
          ] )
    | x -> (builds, x)
  in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let plan = Plan.v () in
  let () =
    List.iter
      (fun build ->
        if
          String_set.is_empty builds_with_deps_set
          || String_set.mem build.Build.name builds_with_deps_set
        then
          let output =
            match output with
            | None -> build.Build.output
            | Some output -> Some Eio.Path.(env#cwd / output)
          in
          let () = Build.add_compile_flags build cflags in
          let () = Build.add_link_flags build ldflags in
          let () =
            Build.add_source_files ~reset:(not (List.is_empty file)) build file
          in
          Plan.build plan
            {
              build with
              pkgconf = build.Build.pkgconf @ pkg;
              ignore = build.Build.ignore @ ignore_patterns;
              output;
              linker =
                Option.map
                  (fun name ->
                    Config.Compiler_config.(
                      linker
                        { name; ext = []; command = None; link_type = "exe" }))
                  linker
                |> Option.value ~default:build.Build.linker;
            })
      x
  in
  Plan.run_all ~execute:run ~args:arg ~log_level plan
    (List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x)

let clean ~path ~builds () =
  Eio_posix.run @@ fun env ->
  if List.is_empty builds then
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#cwd / "zenon-build")
  else
    let x = load_config env path in
    let builds = String_set.of_list builds in
    let x =
      if String_set.is_empty builds then x
      else List.filter (fun b -> String_set.mem b.Build.name builds) x
    in
    List.iter (fun (build : Build.t) -> Build.clean_obj build) x

let cmd_build =
  Cmd.v (Cmd.info "build")
  @@
  let+ output = output
  and+ file = file
  and+ ignore = ignore
  and+ path = path
  and+ builds = targets
  and+ cflags = cflag
  and+ ldflags = ldflag
  and+ arg = arg
  and+ pkg = pkg
  and+ linker = linker
  and+ run = run
  and+ verbose = verbose in
  build ?output ~ignore ~cflags ~ldflags ~path ~builds ~file ~run ~arg ~pkg
    ~linker
    ~log_level:(Util.log_level @@ List.length verbose)
    ()

let cmd_clean =
  Cmd.v (Cmd.info "clean")
  @@
  let+ builds = targets and+ path = path in
  clean ~path ~builds ()

let target =
  let doc = "Target to run" in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"TARGET")

let build =
  let doc = "Compile before running" in
  Arg.(value & flag & info [ "build"; "b" ] ~doc)

let run_args =
  let doc = "Arguments to pass to executable" in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let run ~path ~target ~args ~build () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let b = find_build x target in
  match b with
  | None -> Fmt.failwith "no target found"
  | Some b -> (
      if build then (
        let plan = Plan.v () in
        Plan.build plan b;
        Plan.run_build plan b);
      match b.output with
      | None -> Fmt.failwith "target %s has not output" b.name
      | Some exe ->
          Eio.Process.run env#process_mgr ~executable:(Eio.Path.native_exn exe)
            args)

let cmd_run =
  Cmd.v (Cmd.info "run")
  @@
  let+ target = target
  and+ path = path
  and+ args = run_args
  and+ build = build in
  run ~path ~target ~args ~build ()

let pkg ~path ~prefix ~target ~version ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let b = find_build x target in
  match b with
  | None -> Fmt.failwith "no target found"
  | Some b -> (
      let flags = Flags.concat b.flags (c_flags b) in
      let lib_name_str = lib_name b in
      let contents =
        Pkg_config.generate ~lib_name:lib_name_str ~prefix ~version
          ~requires:b.pkgconf ~cflags:flags.compile ~ldflags:flags.link b.name
      in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_endline contents)

let prefix =
  let doc = "Installation prefix" in
  Arg.(value & opt string "/usr/local" & info [ "prefix" ] ~doc ~docv:"PATH")

let version =
  let doc = "Version" in
  Arg.(value & opt string "0.0.0" & info [ "version" ] ~doc ~docv:"VERSION")

let cmd_gen_pkg_config =
  Cmd.v (Cmd.info "pkg-config")
  @@
  let+ target = target
  and+ path = path
  and+ prefix = prefix
  and+ version = version
  and+ output = output in
  pkg ~path ~target ~prefix ~version ?output ()

let graph ~path ~builds ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let plan = Plan.v () in
  let () =
    List.iter
      (fun build ->
        if
          String_set.is_empty builds_with_deps_set
          || String_set.mem build.Build.name builds_with_deps_set
        then Plan.build plan build)
      x
  in
  Plan.add_dependency_edges plan x;
  let dot = Print.to_dot plan in
  match output with
  | Some path ->
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#cwd / path) dot
  | None -> print_endline dot

let cmd_gen_graph =
  Cmd.v (Cmd.info "graph" ~doc:"Generate graphviz graph of build dependencies")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  graph ~path ~builds ?output ()

let info ~path ~builds () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in
  List.iter
    (fun (b : Build.t) ->
      let sources = Build.locate_source_files b in
      let linker = Linker.auto_select_linker ~sources ~linker:b.linker () in
      Fmt.pr "@[<v 2>Target: %s@," b.name;
      (match b.output with
      | Some p -> Fmt.pr "Output: %s@," (Eio.Path.native_exn p)
      | None -> Fmt.pr "Output: none@,");
      Fmt.pr "Type: %s@,"
        (match linker.link_type with
        | Linker.Executable -> "executable"
        | Linker.Shared -> "shared lib"
        | Linker.Static -> "static lib");
      Fmt.pr "Linker: %s@," linker.name;

      let source_files = Build.locate_source_files b in
      Fmt.pr "Source files: %d@," (List.length source_files);

      List.iter
        (fun (f : Source_file.t) ->
          let ext = Source_file.ext f in
          let compiler_name =
            match Hashtbl.find_opt b.compiler_index ext with
            | Some c -> c.Compiler.name
            | None -> "unknown"
          in
          Fmt.pr "  %s: (compiler %s)@,"
            (Eio.Path.native_exn f.path)
            compiler_name)
        source_files;

      if not (List.is_empty b.depends_on) then
        Fmt.pr "Dependencies: %a@,"
          (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
          b.depends_on
      else Fmt.pr "Dependencies: none@,";

      if not (List.is_empty b.pkgconf) then
        Fmt.pr "Pkg-config: %a@,"
          (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
          b.pkgconf;

      Fmt.pr "@]@.")
    targets

let cmd_info =
  Cmd.v (Cmd.info "info" ~doc:"Show build target information and statistics")
  @@
  let+ builds = targets and+ path = path in
  info ~path ~builds ()

type compile_db_format = CompileCommands | CompileFlags

let compile_commands ~path ~builds ~format ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in

  match format with
  | CompileCommands -> (
      let entries = ref [] in
      List.iter
        (fun (b : Build.t) ->
          let source_files = Build.locate_source_files b in
          let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
          let flags = Flags.v () in
          let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in

          List.iter
            (fun (source : Source_file.t) ->
              let ext = Source_file.ext source in
              match Hashtbl.find_opt b.compiler_index ext with
              | None -> ()
              | Some compiler ->
                  let obj =
                    Object_file.of_source ~root:b.source ~build_name:b.name
                      ~build_dir:Eio.Path.(b.build / "obj")
                      source
                  in
                  let file_flags =
                    Hashtbl.find_opt b.compiler_flags ext
                    |> Option.value ~default:(Flags.v ())
                  in
                  let final_flags = Flags.concat b_flags file_flags in
                  let command =
                    compiler.Compiler.command ~flags:final_flags
                      ~sources:[ source ] ~output:obj
                  in
                  let entry =
                    `O
                      [
                        ("directory", `String (Eio.Path.native_exn env#cwd));
                        ("file", `String (Eio.Path.native_exn source.path));
                        ( "command",
                          `String
                            (String.concat " "
                               (List.map
                                  (fun s ->
                                    if String.contains s ' ' then
                                      Fmt.str "\"%s\"" s
                                    else s)
                                  command)) );
                        ("output", `String (Eio.Path.native_exn obj.path));
                      ]
                  in
                  entries := entry :: !entries)
            source_files)
        targets;

      let json = `A (List.rev !entries) in
      let contents = Ezjsonm.value_to_string ~minify:false json in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_endline contents)
  | CompileFlags -> (
      let flag_set = ref String_set.empty in
      List.iter
        (fun (b : Build.t) ->
          let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
          let flags = Flags.v () in
          let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in
          let source_files = Build.locate_source_files b in

          List.iter
            (fun (source : Source_file.t) ->
              let ext = Source_file.ext source in
              match Hashtbl.find_opt b.compiler_index ext with
              | None -> ()
              | Some _ ->
                  let file_flags =
                    Hashtbl.find_opt b.compiler_flags ext
                    |> Option.value ~default:(Flags.v ())
                  in
                  let final_flags = Flags.concat b_flags file_flags in
                  List.iter
                    (fun flag -> flag_set := String_set.add flag !flag_set)
                    final_flags.compile)
            source_files)
        targets;

      let contents =
        String_set.to_list !flag_set |> String.concat "\n" |> fun s -> s ^ "\n"
      in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_string contents)

let cmd_gen_compile_commands =
  Cmd.v (Cmd.info "compile-commands" ~doc:"Generate compile_commands.json")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  compile_commands ~path ~builds ~format:CompileCommands ?output ()

let cmd_gen_compile_flags =
  Cmd.v (Cmd.info "compile-flags" ~doc:"Generate compile_flags.txt")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  compile_commands ~path ~builds ~format:CompileFlags ?output ()

let cmd_gen =
  Cmd.group
    (Cmd.info "gen" ~doc:"Generate files")
    [
      cmd_gen_compile_commands;
      cmd_gen_compile_flags;
      cmd_gen_pkg_config;
      cmd_gen_graph;
    ]

let install ~path ~builds ~prefix ~version ~uninstall () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in
  let make_install_path subdir = Eio.Path.(env#fs / prefix / subdir) in

  if uninstall then
    (* Uninstall mode *)
    List.iter
      (fun (b : Build.t) ->
        (* Uninstall the built artifact *)
        (match b.output with
        | None -> ()
        | Some output_path -> (
            let sources = Build.locate_source_files b in
            let linker =
              Linker.auto_select_linker ~sources ~linker:b.linker ()
            in
            let install_dir =
              match linker.link_type with
              | Linker.Executable -> make_install_path "bin"
              | Linker.Shared | Linker.Static -> make_install_path "lib"
            in
            let filename =
              Filename.basename (Eio.Path.native_exn output_path)
            in
            let dest = Eio.Path.(install_dir / filename) in

            (* Remove the file *)
            (try
               Eio.Path.unlink dest;
               Util.log "UNINSTALL %s" (Eio.Path.native_exn dest)
             with Eio.Io _ ->
               Util.log "WARNING: File %s not found, skipping"
                 (Eio.Path.native_exn dest));

            (* Uninstall pkg-config file for libraries *)
            match linker.link_type with
            | Linker.Shared | Linker.Static -> (
                let lib_name = lib_name b in
                let pc_dir = make_install_path "lib/pkgconfig" in
                let pc_file = Eio.Path.(pc_dir / (lib_name ^ ".pc")) in
                try
                  Eio.Path.unlink pc_file;
                  Util.log "UNINSTALL %s" (Eio.Path.native_exn pc_file)
                with Eio.Io _ ->
                  Util.log "WARNING: File %s not found, skipping"
                    (Eio.Path.native_exn pc_file))
            | Linker.Executable -> ()));

        (* Uninstall header files *)
        let headers = Build.locate_headers b in
        if not (List.is_empty headers) then (
          let include_name = lib_name b in
          let include_dir = make_install_path ("include/" ^ include_name) in
          List.iter
            (fun header_path ->
              let rel_path = Util.relative_to b.source header_path in
              let dest = Eio.Path.(include_dir / rel_path) in
              try
                Eio.Path.unlink dest;
                Util.log "UNINSTALL %s" (Eio.Path.native_exn dest)
              with Eio.Io _ ->
                Util.log "WARNING: File %s not found, skipping"
                  (Eio.Path.native_exn dest))
            headers;
          (* Try to remove the include directory if it's empty *)
          try Eio.Path.rmdir include_dir with Eio.Io _ -> ()))
      targets
  else
    (* Install mode *)
    List.iter
      (fun (b : Build.t) ->
        (* Install the built artifact *)
        (match b.output with
        | None -> Util.log "Skipping %s (no output)" b.name
        | Some output_path -> (
            (* Check if output file exists *)
            match Eio.Path.kind ~follow:true output_path with
            | `Regular_file -> (
                let sources = Build.locate_source_files b in
                let linker =
                  Linker.auto_select_linker ~sources ~linker:b.linker ()
                in
                let install_dir =
                  match linker.link_type with
                  | Linker.Executable -> make_install_path "bin"
                  | Linker.Shared | Linker.Static -> make_install_path "lib"
                in
                Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 install_dir;
                let filename =
                  Filename.basename (Eio.Path.native_exn output_path)
                in
                let dest = Eio.Path.(install_dir / filename) in

                (* Copy the file *)
                Util.log "INSTALL %s -> %s"
                  (Eio.Path.native_exn output_path)
                  (Eio.Path.native_exn dest);
                let contents = Eio.Path.load output_path in
                Eio.Path.save ~create:(`Or_truncate 0o755) dest contents;

                (* Install pkg-config file for libraries *)
                match linker.link_type with
                | Linker.Shared | Linker.Static ->
                    let lib_name = lib_name b in
                    let c_flags = c_flags b in
                    let flags = Flags.concat b.flags c_flags in
                    let pc_contents =
                      Pkg_config.generate ~lib_name ~prefix ~version
                        ~requires:b.pkgconf ~cflags:flags.compile
                        ~ldflags:flags.link b.name
                    in
                    let pc_dir = make_install_path "lib/pkgconfig" in
                    Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true pc_dir;
                    let pc_file = Eio.Path.(pc_dir / (lib_name ^ ".pc")) in
                    Util.log "INSTALL pkg-config -> %s"
                      (Eio.Path.native_exn pc_file);
                    Eio.Path.save ~create:(`Or_truncate 0o644) pc_file
                      pc_contents
                | Linker.Executable -> ())
            | _ ->
                Util.log "WARNING: Output file %s not found, skipping"
                  (Eio.Path.native_exn output_path)
            | exception Eio.Io _ ->
                Util.log "WARNING: Output file %s not found, skipping"
                  (Eio.Path.native_exn output_path)));

        (* Install header files *)
        let headers = Build.locate_headers b in
        if not (List.is_empty headers) then (
          let include_name = lib_name b in
          let include_dir = make_install_path ("include/" ^ include_name) in
          Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true include_dir;
          List.iter
            (fun header_path ->
              let rel_path = Util.relative_to b.source header_path in
              let dest = Eio.Path.(include_dir / rel_path) in
              let dest_dir =
                Eio.Path.(include_dir / Filename.dirname rel_path)
              in

              Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true dest_dir;

              Util.log "INSTALL %s -> %s"
                (Eio.Path.native_exn header_path)
                (Eio.Path.native_exn dest);
              let contents = Eio.Path.load header_path in
              Eio.Path.save ~create:(`Or_truncate 0o644) dest contents)
            headers))
      targets

let uninstall_flag =
  let doc = "Uninstall artifacts instead of installing" in
  Arg.(value & flag & info [ "u"; "uninstall" ] ~doc)

let cmd_install =
  Cmd.v
    (Cmd.info "install"
       ~doc:
         "Install or uninstall built artifacts to/from specified prefix \
          directory")
  @@
  let+ builds = targets
  and+ path = path
  and+ prefix = prefix
  and+ version = version
  and+ uninstall = uninstall_flag in
  install ~path ~builds ~prefix ~version ~uninstall ()

let list_tools ~path () =
  Eio_posix.run @@ fun env ->
  (* Load config to register custom compilers/linkers *)
  (match Config.load ~env Eio.Path.(env#fs / path) with
  | Ok _ -> ()
  | Error (`Msg err) -> Util.log "WARNING failed to load config: %s" err);

  (* Default compilers/linkers enabled by default *)
  let default_compiler_names =
    List.map (fun c -> c.Config.Compiler_config.name) Config.default_compilers
    |> String_set.of_list
  in
  let default_linker_names =
    List.map (fun c -> c.Config.Compiler_config.name) Config.default_linkers
    |> String_set.of_list
  in

  (* Display compilers (includes custom ones registered from config) *)
  Fmt.pr "@[<v>Compilers:@,";
  List.iter
    (fun (c : Compiler.t) ->
      let is_default = String_set.mem c.name default_compiler_names in
      let marks = if is_default then " [default]" else "" in
      match Command.path env#process_mgr c.name with
      | Some path ->
          let exts = String_set.to_list c.ext |> String.concat ", " in
          Fmt.pr "  \u{2713} %s (%s) - %s%s@," c.name path exts marks
      | None ->
          let exts = String_set.to_list c.ext |> String.concat ", " in
          Fmt.pr "  \u{2717} %s - %s%s@," c.name exts marks)
    !Compiler.all;

  (* Display linkers (includes custom ones registered from config) *)
  Fmt.pr "@,Linkers:@,";
  List.iter
    (fun (l : Linker.t) ->
      let is_default = String_set.mem l.name default_linker_names in
      let marks = if is_default then " [default]" else "" in
      let link_type =
        match l.link_type with
        | Linker.Executable -> "exe"
        | Linker.Shared -> "shared"
        | Linker.Static -> "static"
      in
      match Command.path env#process_mgr l.name with
      | Some path ->
          Fmt.pr "  \u{2713} %s (%s) [%s]%s@," l.name path link_type marks
      | None -> Fmt.pr "  \u{2717} %s [%s]%s@," l.name link_type marks)
    !Linker.all;
  Fmt.pr "@]"

let cmd_tools =
  Cmd.v (Cmd.info "tools" ~doc:"List available compilers and linkers")
  @@
  let+ path = path in
  list_tools ~path ()

let main () =
  try
    Cmd.eval ~catch:false
    @@ Cmd.group (Cmd.info "zenon")
         [
           cmd_build;
           cmd_run;
           cmd_clean;
           cmd_info;
           cmd_gen;
           cmd_install;
           cmd_tools;
         ]
  with
  | Invalid_argument msg ->
      Util.log "ERROR invalid argument: %s\n" msg;
      1
  | Failure msg ->
      Util.log "ERROR failure: %s\n" msg;
      2
  | Eio.Exn.Multiple _exn -> 2
  | exn ->
      Util.log "ERROR exception: %a\n" Fmt.exn exn;
      -1

let () = if !Sys.interactive then () else exit (main ())
