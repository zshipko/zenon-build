let with_log_file ?(keep = false) ~build_dir ~name f =
  let logs_dir = Eio.Path.(build_dir / "logs") in
  Eio.Path.mkdirs ~exists_ok:true logs_dir ~perm:0o755;
  let tmp_path = Eio.Path.(logs_dir / name) in
  Fun.protect ~finally:(fun () -> if not keep then Eio.Path.unlink tmp_path)
  @@ fun () ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp_path @@ fun file ->
  f (tmp_path, file)

let get ?(unlink = false) log_path =
  let log = Eio.Path.load log_path in
  if unlink then Eio.Path.unlink log_path;
  log
