type 'a t = { checked : (string, bool) Hashtbl.t; mgr : 'a Eio.Process.mgr }

let v mgr = { checked = Hashtbl.create 16; mgr }

let path mgr cmd =
  try
    let result = Eio.Process.parse_out mgr Eio.Buf_read.line [ "which"; cmd ] in
    let path = String.trim result in
    if Sys.file_exists path then Some path else None
  with _ -> None

let is_available t cmd =
  match Hashtbl.find_opt t.checked cmd with
  | Some result -> result
  | None ->
      let available = path t.mgr cmd |> Option.is_some in
      Hashtbl.add t.checked cmd available;
      available

let check_command t cmd =
  if not (is_available t cmd) then
    Fmt.failwith "command '%s' not found. Please install it and try again." cmd

let check_commands t cmds =
  let missing =
    List.fold_left
      (fun acc cmd -> if not (is_available t cmd) then cmd :: acc else acc)
      [] cmds
  in
  if not (List.is_empty missing) then
    Fmt.failwith "missing commands: %a@."
      (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
      (List.rev missing)
