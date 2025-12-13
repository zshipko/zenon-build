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
  Arg.(value & opt_all string [] & info [ "cflag" ] ~doc ~docv:"FLAG")

let ldflag =
  let doc = "Linker flag" in
  Arg.(value & opt_all string [] & info [ "ldflag" ] ~doc ~docv:"FLAG")

let main ?output cflags ldflags path =
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

let cmd_main =
  Cmd.v (Cmd.info "zenon")
  @@
  let+ output = output
  and+ path = path
  and+ cflag = cflag
  and+ ldflag = ldflag in
  main ?output cflag ldflag path

let main () = Cmd.eval cmd_main
let () = if !Sys.interactive then () else exit (main ())
