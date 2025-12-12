let () =
  Eio_posix.run @@ fun env ->
  let source = Eio.Path.(env#cwd / "test") in
  let output = Eio.Path.(source / "a.out") in
  let z = Zenon.Build.v ~source ~output env in
  Zenon.Flags.add_link_flags z.Zenon.Build.flags [ "-ccopt"; "-lsqlite3" ];
  Zenon.Build.detect_source_files z [ "c"; "ml" ];
  Zenon.Build.run z
