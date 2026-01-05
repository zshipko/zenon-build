type path = Eio.Fs.dir_ty Eio.Path.t

module String_set = Set.Make (String)

module Path_set = Set.Make (struct
  type t = path

  let compare a b =
    let a = Eio.Path.native_exn a in
    let b = Eio.Path.native_exn b in
    String.compare a b
end)

module Source_file = struct
  type t = { path : path; flags : Flags.t; root : path }

  let v ?flags ~root path =
    { path; flags = Option.value ~default:(Flags.v ()) flags; root }

  let ext { path; _ } = Util.ext path
end

module Object_file = struct
  type t = { source : Source_file.t; path : path; flags : Flags.t }

  let v ?flags ~source path =
    { source; path; flags = Option.value ~default:(Flags.v ()) flags }

  let of_source ?flags ~root ~build_name ~build_dir source =
    let obj_file = Util.relative_to root source.Source_file.path ^ ".o" in
    v ?flags ~source @@ Eio.Path.(build_dir / build_name / obj_file)
end
