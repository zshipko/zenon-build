type t = { fmt : Format.formatter; buffer : Buffer.t }

let v () =
  let buffer = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buffer in
  { buffer; fmt }

let contents t =
  Format.pp_print_flush t.fmt ();
  Buffer.contents t.buffer

let add_comment t text =
  Fmt.pf t.fmt "# %s@." text

let add_variable t name value =
  Fmt.pf t.fmt "%s = %s@." name value

let add_target t ~name ~deps ~commands =
  Fmt.pf t.fmt "@.%s:%a@." name
    (Fmt.list ~sep:(Fmt.any " ") Fmt.string)
    (if List.is_empty deps then [] else " " :: deps);
  List.iter (fun cmd -> Fmt.pf t.fmt "\t%s@." cmd) commands

let add_phony t targets =
  if not (List.is_empty targets) then
    Fmt.pf t.fmt "@..PHONY:%a@."
      (Fmt.list ~sep:(Fmt.any " ") Fmt.string)
      (" " :: targets)
