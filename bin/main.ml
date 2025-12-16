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
  Arg.(
    value
    & (opt_all string
      @@ Zenon.String_set.(
           map (fun x -> "*." ^ x) Zenon.Compiler_set.default_ext |> to_list))
    & info [ "file"; "f" ] ~doc ~docv:"FILE")

let build ?output ?(ignore = []) ~cflags ~ldflags ~path ~builds ~file () =
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
            Zenon.Build.v env ~ignore
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
          Zenon.Plan.build plan build)
      x
  in
  Zenon.Plan.run_all plan
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
  and+ ldflags = ldflag in
  build ?output ~ignore ~cflags ~ldflags ~path ~builds ~file ()

let cmd_clean =
  Cmd.v (Cmd.info "clean")
  @@
  let+ builds = builds and+ path = path in
  clean ~path ~builds ()

let main () = Cmd.eval @@ Cmd.group (Cmd.info "zenon") [ cmd_build; cmd_clean ]
let () = if !Sys.interactive then () else exit (main ())
