open Zenon

let rec collect_dependencies build_map name visited =
  if String_set.mem name visited then visited
  else
    let visited = String_set.add name visited in
    match Hashtbl.find_opt build_map name with
    | None -> visited
    | Some build ->
        List.fold_left
          (fun acc dep -> collect_dependencies build_map dep acc)
          visited build.Build.depends_on

let load_config env path =
  let normalized_path = Unix.realpath path in
  match Config.load ~env Eio.Path.(env#fs / normalized_path) with
  | Ok x -> x
  | Error (`Msg err) -> failwith err

let find_build builds name =
  match name with
  | Some name -> List.find_opt (fun b -> b.Build.name = name) builds
  | None -> ( try Some (List.hd builds) with _ -> None)

let lib_name b =
  let name = b.Build.name in
  let name =
    if String.starts_with ~prefix:"lib" name then
      String.sub name 3 (String.length name - 3)
    else name
  in
  Filename.remove_extension name

let c_flags b =
  Hashtbl.find_opt b.Build.compiler_flags "c"
  |> Option.value ~default:(Flags.v ())

let filter_builds builds names =
  if List.is_empty names then
    List.filter_map
      (fun x -> if x.Build.hidden then None else Some x.Build.name)
      builds
  else names

let make_build_map builds =
  List.fold_left
    (fun acc b ->
      Hashtbl.add acc b.Build.name b;
      acc)
    (Hashtbl.create (List.length builds))
    builds

let builds_with_deps build_map names =
  List.fold_left
    (fun acc name -> collect_dependencies build_map name acc)
    String_set.empty names
