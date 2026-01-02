open Types

type t = { source : Source_file.t; path : path; flags : Flags.t }

let v ?flags ~source path =
  { source; path; flags = Option.value ~default:(Flags.v ()) flags }

let of_source ?flags ~root ~build_name ~build_dir source =
  let obj_file = Util.relative_to root source.Source_file.path ^ ".o" in
  v ?flags ~source @@ Eio.Path.(build_dir / build_name / obj_file)
