open Zenon
open Common

let info ~path ~builds () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Quiet ~builds env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in
  List.iter
    (fun (b : Build.t) ->
      let sources = Build.locate_source_files b |> List.of_seq in
      let linker = Linker.auto_select_linker ~sources ~linker:b.linker b.name in
      Fmt.pr "@[<v 2>Target: %s@," b.name;
      (match b.output with
      | Some p -> Fmt.pr "Output: %s@," (Eio.Path.native_exn p)
      | None -> Fmt.pr "Output: none@,");
      Fmt.pr "Type: %s@,"
        (match linker.link_type with
        | Linker.Executable -> "executable"
        | Linker.Shared -> "shared lib"
        | Linker.Static -> "static lib");
      Fmt.pr "Linker: %s@," linker.name;

      Fmt.pr "Source files: %d@," (List.length sources);

      List.iter
        (fun (f : Source_file.t) ->
          let ext = Source_file.ext f in
          let compiler_name =
            match Hashtbl.find_opt b.compiler_index ext with
            | Some c -> c.Compiler.name
            | None -> "unknown"
          in
          Fmt.pr "  %s: (compiler %s)@,"
            (Eio.Path.native_exn f.path)
            compiler_name)
        sources;

      if not (List.is_empty b.depends_on) then
        Fmt.pr "Dependencies: %a@,"
          (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
          b.depends_on
      else Fmt.pr "Dependencies: none@,";

      if not (List.is_empty b.pkgconf) then
        Fmt.pr "Pkg-config: %a@,"
          (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
          b.pkgconf;

      Fmt.pr "@]@.")
    targets
