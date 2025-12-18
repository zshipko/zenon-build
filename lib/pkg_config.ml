let cflags ~env names =
  Eio.Process.parse_out env#process_mgr Eio.Buf_read.line
    ([ "pkg-config"; "--cflags" ] @ names)
  |> String.split_on_char ' '

let ldflags ~env names =
  Eio.Process.parse_out env#process_mgr Eio.Buf_read.line
    ([ "pkg-config"; "--libs" ] @ names)
  |> String.split_on_char ' '

let flags ~env names =
  let compile = cflags ~env names in
  let link = ldflags ~env names in
  Compiler.Flags.v ~compile ~link ()
