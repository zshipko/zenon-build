let () =
  Eio_posix.run @@ fun env ->
  let source = env#cwd in
  let file = Eio.Path.load Eio.Path.(source / "zenon.yaml") in
  let config =
    match Zenon.Config.of_yaml (Yaml.of_string_exn file) with
    | Ok x -> x
    | Error (`Msg msg) -> failwith msg
  in
  (* let config = *)
  (* Zenon.Config. *)
  (* { *)
  (* cflags = []; *)
  (* ldflags = []; *)
  (* build = *)
  (* [ *)
  (* Zenon.Config.Build_config. *)
  (* { *)
  (* name = None; *)
  (* import = []; *)
  (* output = "test"; *)
  (* compilers = [ "cc"; "ocaml" ]; *)
  (* linker = "ocaml"; *)
  (* detect_source = [ "c"; "ml" ]; *)
  (* cflags = []; *)
  (* ldflags = []; *)
  (* files = []; *)
  (* }; *)
  (* ]; *)
  (* root = Eio.Path.native_exn source; *)
  (* } *)
  (* in *)
  let w = Zenon.Config.workspace config ~env source in
  Zenon.Workspace.run w
(* Zenon.Workspace.clean_obj w *)
