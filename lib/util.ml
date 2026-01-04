let lock = Mutex.create ()

let log ?(verbose = true) fmt =
  Mutex.protect lock @@ fun () ->
  if verbose then Fmt.epr (fmt ^^ "@.") else Fmt.kstr ignore fmt

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

let gitignore_to_glob pattern =
  (* gitignore patterns should not get **/ prefix if they contain / *)
  if String.contains pattern '/' then glob pattern else glob_path pattern

let is_static_lib (filename : string) =
  String.starts_with ~prefix:"lib" filename
  && String.ends_with ~suffix:".a" filename

let is_shared_lib (filename : string) =
  String.starts_with ~prefix:"lib" filename
  && (String.ends_with ~suffix:".so" filename
     || String.ends_with ~suffix:".dylib" filename)

let normalize_shared_lib_ext (filename : string) =
  if Sys.os_type = "Unix" && String.ends_with ~suffix:".so" filename then
    try
      let ic = Unix.open_process_in "uname -s" in

      let uname =
        Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
        String.trim @@ input_line ic
      in
      if uname = "Darwin" then Filename.remove_extension filename ^ ".dylib"
      else filename
    with _ -> filename
  else filename

let parse_gitignore path =
  if Eio.Path.is_file path then
    Eio.Path.with_lines path @@ fun lines ->
    Seq.filter_map
      (fun line ->
        let line = String.trim line in
        if String.length line = 0 || String.starts_with ~prefix:"#" line then
          None
        else
          let pattern =
            if String.starts_with ~prefix:"/" line then
              (* Remove leading / for anchored patterns *)
              String.sub line 1 (String.length line - 1)
            else line
          in
          Some (gitignore_to_glob pattern))
      lines
    |> List.of_seq
  else []
