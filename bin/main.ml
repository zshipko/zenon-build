open Cmdliner
open Cmdliner.Term.Syntax
open Zenon
open Flag
open Common

let clean ~path ~log_level ~builds () =
  Eio_posix.run @@ fun env ->
  if List.is_empty builds then
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#cwd / "zenon-build")
  else
    let x = load_config ~log_level ~builds env path in
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
  and+ verbose = verbose
  and+ do_clean = clean_build in
  let verbosity_level =
    if not (Unix.isatty Unix.stderr) then max 1 (List.length verbose)
    else List.length verbose
  in
  let log_level = Util.log_level verbosity_level in
  if do_clean then clean ~log_level ~path ~builds ();
  Build_command.build ?output ~ignore ~cflags ~ldflags ~path ~builds ~file ~run
    ~arg ~pkg ~linker ~log_level ()

let cmd_clean =
  Cmd.v (Cmd.info "clean")
  @@
  let+ builds = targets and+ path = path and+ verbose = verbose in
  let verbosity_level =
    if not (Unix.isatty Unix.stderr) then max 1 (List.length verbose)
    else List.length verbose
  in
  let log_level = Util.log_level verbosity_level in
  clean ~log_level ~path ~builds ()

let run ~path ~target ~args ~build () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Info ~builds:[ build ] env path in
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

let cmd_gen_pkg_config =
  Cmd.v (Cmd.info "pkg-config")
  @@
  let+ target = target
  and+ path = path
  and+ prefix = prefix
  and+ version = version
  and+ output = output in
  Gen_command.pkg ~path ~target ~prefix ~version ?output ()

let cmd_gen_graph =
  Cmd.v (Cmd.info "graph" ~doc:"Generate graphviz graph of build dependencies")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  Gen_command.graph ~path ~builds ?output ()

let cmd_info =
  Cmd.v (Cmd.info "info" ~doc:"Show build target information and statistics")
  @@
  let+ builds = targets and+ path = path in
  Info_command.info ~path ~builds ()

let cmd_gen_compile_commands =
  Cmd.v (Cmd.info "compile-commands" ~doc:"Generate compile_commands.json")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  Gen_command.compile_commands ~path ~builds ~format:Gen_command.CompileCommands
    ?output ()

let cmd_gen_compile_flags =
  Cmd.v (Cmd.info "compile-flags" ~doc:"Generate compile_flags.txt")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  Gen_command.compile_commands ~path ~builds ~format:Gen_command.CompileFlags
    ?output ()

let cmd_gen_gitignore =
  Cmd.v (Cmd.info "gitignore" ~doc:"Generate .gitignore with build outputs")
  @@
  let+ builds = targets and+ path = path and+ output = output in
  Gen_command.gitignore ~path ~builds ?output ()

let cmd_gen =
  Cmd.group
    (Cmd.info "gen" ~doc:"Generate files")
    [
      cmd_gen_compile_commands;
      cmd_gen_compile_flags;
      cmd_gen_pkg_config;
      cmd_gen_graph;
      cmd_gen_gitignore;
    ]

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
  and+ uninstall = uninstall in
  Install_command.install ~path ~builds ~prefix ~version ~uninstall ()

let list_tools ~path () =
  Eio_posix.run @@ fun env ->
  (* Load config to register custom compilers/linkers *)
  (match Config.load ~log_level:`Quiet ~env Eio.Path.(env#fs / path) with
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
