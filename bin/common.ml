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

let filter_builds builds names =
  if List.is_empty names then
    List.filter_map
      (fun x -> if x.Build.hidden then None else Some x.Build.name)
      builds
  else names

let filter_builds_for_cwd rel_path builds x =
  match rel_path with
  | Some rel when List.is_empty builds ->
      let abs_rel = Unix.realpath rel in
      List.filter
        (fun b ->
          let source_path = Eio.Path.native_exn b.Build.source in
          let abs_source = Unix.realpath source_path in
          String.equal abs_source abs_rel)
        x
  | _ -> x

let load_config ~builds env path : Build.t list =
  let normalized_path = Unix.realpath path in
  let eio_path = Eio.Path.(env#fs / normalized_path) in

  (* Find the directory containing the config file by searching upwards *)
  let dir_path =
    if Eio.Path.is_directory eio_path then eio_path
    else
      match Eio.Path.split eio_path with None -> eio_path | Some (p, _) -> p
  in

  let project_root =
    match Config.find_config_in_parents dir_path with
    | Some config_path -> (
        match Eio.Path.split config_path with
        | None -> normalized_path
        | Some (dir, _) -> Eio.Path.native_exn dir)
    | None -> normalized_path
  in

  let project_root_path = Eio.Path.(env#fs / project_root) in

  (* Calculate the relative path from project root to original directory *)
  let rel_path =
    if String.equal normalized_path project_root then None
    else try Some (Util.relative_to project_root_path dir_path) with _ -> None
  in

  Sys.chdir project_root;

  match Config.load ~env project_root_path with
  | Ok x -> filter_builds_for_cwd rel_path builds x
  | Error (`Msg err) -> failwith err

let find_build builds name =
  match (name, builds) with
  | Some name, builds -> List.find_opt (fun b -> b.Build.name = name) builds
  | None, [] -> None
  | None, h :: _ -> Some h

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
