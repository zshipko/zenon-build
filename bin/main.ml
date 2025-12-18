open Cmdliner
open Cmdliner.Term.Syntax

let path =
  let doc = "Root directory" in
  Arg.(
    value & opt string (Sys.getcwd ()) & info [ "p"; "path" ] ~doc ~docv:"PATH")

let output =
  let doc = "Output file name" in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv:"PATH")

let builds =
  let doc = "Selected builds" in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"BUILD")

let cflag =
  let doc = "Compiler flag" in
  Arg.(value & opt_all string [] & info [ "cflag" ] ~doc ~docv:"FLAG")

let ldflag =
  let doc = "Linker flag" in
  Arg.(value & opt_all string [] & info [ "ldflag" ] ~doc ~docv:"FLAG")

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

let build ?output ?(ignore = []) ~arg ~cflags ~ldflags ~path ~builds ~file ~run
    ~pkg () =
  Eio_posix.run @@ fun env ->
  let x =
    match Zenon.Config.load ~env Eio.Path.(env#fs / path) with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  let builds, x =
    match x with
    | [] ->
        ( [ "default" ],
          [
            Zenon.Build.v env ~ignore ~pkgconf:pkg
              ~flags:(Zenon.Flags.v ~compile:cflags ~link:ldflags ())
              ~source:Eio.Path.(env#fs / path)
              ~files:file ~name:"default";
          ] )
    | x -> (builds, x)
  in
  let builds =
    if List.is_empty builds then List.map (fun x -> x.Zenon.Build.name) x
    else builds
  in
  let builds = Zenon.String_set.of_list builds in
  let plan = Zenon.Plan.v () in
  let () =
    List.iter
      (fun build ->
        if
          Zenon.String_set.is_empty builds
          || Zenon.String_set.mem build.Zenon.Build.name builds
        then
          let () =
            match output with
            | None -> ()
            | Some output ->
                build.Zenon.Build.output <- Some Eio.Path.(env#cwd / output)
          in
          let () = Zenon.Build.add_compile_flags build cflags in
          let () = Zenon.Build.add_link_flags build ldflags in
          let () = Zenon.Build.add_source_files build file in
          let () =
            build.Zenon.Build.ignore <-
              build.Zenon.Build.ignore @ List.map Zenon.Util.glob ignore
          in
          let () =
            build.Zenon.Build.pkgconf <- build.Zenon.Build.pkgconf @ pkg
          in
          Zenon.Plan.build plan build)
      x
  in
  Zenon.Plan.run_all ~execute:run ~args:arg plan
    (List.filter (fun b -> Zenon.String_set.mem b.Zenon.Build.name builds) x)

let clean ~path ~builds () =
  Eio_posix.run @@ fun env ->
  let path = Eio.Path.(env#fs / path) in
  let x =
    match Zenon.Config.load ~env path with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  let builds = Zenon.String_set.of_list builds in
  let x =
    if Zenon.String_set.is_empty builds then x
    else List.filter (fun b -> Zenon.String_set.mem b.Zenon.Build.name builds) x
  in
  List.iter (fun (build : Zenon.Build.t) -> Zenon.Build.clean build) x

let cmd_build =
  Cmd.v (Cmd.info "build")
  @@
  let+ output = output
  and+ file = file
  and+ ignore = ignore
  and+ path = path
  and+ builds = builds
  and+ cflags = cflag
  and+ ldflags = ldflag
  and+ arg = arg
  and+ pkg = pkg
  and+ run = run in
  build ?output ~ignore ~cflags ~ldflags ~path ~builds ~file ~run ~arg ~pkg ()

let cmd_clean =
  Cmd.v (Cmd.info "clean")
  @@
  let+ builds = builds and+ path = path in
  clean ~path ~builds ()

let build =
  let doc = "Build output to run" in
  Arg.(value & pos 0 string "" & info [] ~doc ~docv:"BUILD")

let run_args =
  let doc = "Arguments to pass to executable" in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let run ~path ~build ~args () =
  Eio_posix.run @@ fun env ->
  let path = Eio.Path.(env#fs / path) in
  let x =
    match Zenon.Config.load ~env path with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  match List.find_opt (fun b -> b.Zenon.Build.name = build) x with
  | None -> Fmt.failwith "unable to find target %s\n" build
  | Some b -> (
      match b.output with
      | None -> Fmt.failwith "target %s has not output" build
      | Some exe ->
          Eio.Process.run env#process_mgr ~executable:(Eio.Path.native_exn exe)
            args)

let cmd_run =
  Cmd.v (Cmd.info "run")
  @@
  let+ build = build and+ path = path and+ args = run_args in
  run ~path ~build ~args ()

let main () =
  Cmd.eval @@ Cmd.group (Cmd.info "zenon") [ cmd_build; cmd_run; cmd_clean ]

let () = if !Sys.interactive then () else exit (main ())
