type 'a t = { checked : (string, bool) Hashtbl.t; mgr : 'a Eio.Process.mgr }

let v mgr = { checked = Hashtbl.create 16; mgr }

let is_available t cmd =
  match Hashtbl.find_opt t.checked cmd with
  | Some result -> result
  | None ->
      let available =
        try
          let result =
            Eio.Process.parse_out t.mgr Eio.Buf_read.line [ "which"; cmd ]
          in
          not (String.equal result "")
        with _ -> false
      in
      Hashtbl.add t.checked cmd available;
      available

let check_command t cmd =
  if not (is_available t cmd) then
    Fmt.failwith "Command '%s' not found. Please install it and try again." cmd

let check_commands t cmds =
  let missing = ref [] in
  List.iter
    (fun cmd -> if not (is_available t cmd) then missing := cmd :: !missing)
    cmds;
  if not (List.is_empty !missing) then
    Fmt.failwith
      "The following commands are not available: %a@.Please install them and \
       try again."
      (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
      (List.rev !missing)
