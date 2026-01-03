let parse_line line : string list =
  let line = String.trim line in
  let buf = Buffer.create 32 in
  let in_quote = ref false in
  let in_dquote = ref false in
  let next acc =
    in_quote := false;
    in_dquote := false;
    if Buffer.length buf = 0 then acc
    else
      let s = Buffer.contents buf in
      let () = Buffer.clear buf in
      s :: acc
  in
  let x =
    String.fold_left
      (fun acc -> function
        | (' ' | '\t') when not (!in_quote && !in_dquote) -> next acc
        | '"' when !in_dquote && not !in_quote -> next acc
        | '\'' when !in_quote && not !in_dquote -> next acc
        | '\'' when not !in_dquote ->
            in_quote := true;
            acc
        | '"' when not !in_dquote ->
            in_dquote := true;
            acc
        | c ->
            Buffer.add_char buf c;
            acc)
      [] line
  in
  next x

let cflags ~env names =
  if List.is_empty names then []
  else
    try
      Eio.Process.parse_out env#process_mgr Eio.Buf_read.line
        ([ "pkg-config"; "--cflags" ] @ names)
      |> parse_line
    with _ ->
      Fmt.failwith "unable to find pkg-config package %a"
        (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string)
        names

let ldflags ~env names =
  if List.is_empty names then []
  else
    try
      Eio.Process.parse_out env#process_mgr Eio.Buf_read.line
        ([ "pkg-config"; "--libs" ] @ names)
      |> parse_line
    with _ ->
      Fmt.failwith "unable to find pkg-config package %a"
        (Fmt.list ~sep:(Fmt.const Fmt.string ", ") Fmt.string)
        names

let flags ~env names =
  let compile = cflags ~env names in
  let link = ldflags ~env names in
  Flags.v ~compile ~link ()

let generate ?(prefix = "/usr/local") ?(version = "0.0.0")
    ?(include_dir = "include") ?(lib_dir = "lib") ?(requires = []) ~lib_name
    name ~cflags ~ldflags =
  Fmt.str
    "prefix=%s\n\
     includedir=${prefix}/%s/%s\n\
     libdir=${prefix}/%s\n\n\
     Name: %s\n\
     Description: %s\n\
     Version: %s\n\
     Cflags: -I${includedir} %a\n\
     Libs: -L${libdir} -l%s %a\n\
     Requires: %a\n"
    prefix include_dir lib_name lib_dir name name version
    (Fmt.list ~sep:(Fmt.const Fmt.string " ") Fmt.string)
    cflags lib_name
    (Fmt.list ~sep:(Fmt.const Fmt.string " ") Fmt.string)
    ldflags
    (Fmt.list ~sep:(Fmt.const Fmt.string " ") Fmt.string)
    requires
