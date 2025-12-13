open Cmdliner
open Cmdliner.Term.Syntax

let output =
  let doc = "Output file name" in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv:"PATH")

let path =
  let doc = "Path to build" in
  Arg.(value & pos 0 string "." & info [] ~doc ~docv:"PATH")

let cflag =
  let doc = "Compiler flag" in
  Arg.(
    value & opt_all string [] & info [ "flag:compile"; "fc" ] ~doc ~docv:"FLAG")

let ldflag =
  let doc = "Linker flag" in
  Arg.(value & opt_all string [] & info [ "flag:link"; "fl" ] ~doc ~docv:"FLAG")

let build ?output cflags ldflags path =
  Eio_posix.run @@ fun env ->
  let path = Eio.Path.(env#cwd / path) in
  let x =
    match Zenon.Config.load ~env path with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  List.iter
    (fun (build : Zenon.Build.t) ->
      let () =
        match output with
        | None -> ()
        | Some output -> build.output <- Eio.Path.(env#cwd / output)
      in
      Zenon.Build.add_compile_flags build cflags;
      Zenon.Build.add_link_flags build ldflags;
      Zenon.Build.run build)
    x

let clean path =
  Eio_posix.run @@ fun env ->
  let path = Eio.Path.(env#cwd / path) in
  let x =
    match Zenon.Config.load ~env path with
    | Ok x -> x
    | Error (`Msg err) -> failwith err
  in
  List.iter (fun (build : Zenon.Build.t) -> Zenon.Build.clean build) x

let cmd_build =
  Cmd.v (Cmd.info "build")
  @@
  let+ output = output
  and+ path = path
  and+ cflag = cflag
  and+ ldflag = ldflag in
  build ?output cflag ldflag path

let cmd_clean =
  Cmd.v (Cmd.info "clean")
  @@
  let+ path = path in
  clean path

let main () = Cmd.eval @@ Cmd.group (Cmd.info "zenon") [ cmd_build; cmd_clean ]
let () = if !Sys.interactive then () else exit (main ())
