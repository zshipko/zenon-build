open Types

type t = { path : path; flags : Flags.t; root : path }

let v ?flags ~root path =
  { path; flags = Option.value ~default:(Flags.v ()) flags; root }

let ext { path; _ } = Util.ext path
