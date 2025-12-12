let () =
  Eio_posix.run @@ fun env ->
  let source = Eio.Path.(env#cwd / "test") in
  let z = Zenon.v ~source env in
  Zenon.detect_source_files z [ "c"; "ml" ];
  Zenon.run z
