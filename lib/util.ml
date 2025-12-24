let log fmt = Fmt.epr (fmt ^^ "\n%!")

let ext path =
  let s =
    Eio.Path.split path |> Option.map snd |> Option.value ~default:""
    |> Filename.extension
  in
  if String.length s > 0 then String.sub s 1 (String.length s - 1) else ""

let with_ext path ext =
  let a, b = Eio.Path.split path |> Option.get in
  let c = Filename.remove_extension b ^ "." ^ ext in
  Eio.Path.(a / c)

let mkparent path =
  let parent = Eio.Path.split path |> Option.map fst in
  Option.iter
    (fun p ->
      if Eio.Path.native_exn p = "." || Eio.Path.native_exn p = "" then ()
      else if not (Eio.Path.is_directory p) then
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p)
    parent

let relative_to base a =
  let prefix = Eio.Path.native_exn base in
  let a = Eio.Path.native_exn a in
  if String.starts_with ~prefix a then
    let prefix_len = String.length prefix in
    String.sub a (prefix_len + 1) (String.length a - prefix_len - 1)
  else a

let glob =
  Re.Glob.glob ~pathname:true ~anchored:true ~double_asterisk:true
    ~expand_braces:true

let glob_path path = glob (Filename.concat "**" path)
