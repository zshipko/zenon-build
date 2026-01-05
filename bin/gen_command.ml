open Zenon
open Common

let pkg ~path ~prefix ~target ~version ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let b = find_build x target in
  match b with
  | None -> Fmt.failwith "no target found"
  | Some b -> (
      let flags = Flags.concat b.flags (c_flags b) in
      let lib_name_str = lib_name b in
      let contents =
        Pkg_config.generate ~lib_name:lib_name_str ~prefix ~version
          ~requires:b.pkgconf ~cflags:flags.compile ~ldflags:flags.link b.name
      in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_endline contents)

let graph ~path ~builds ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let plan = Plan.v () in
  let () =
    List.iter
      (fun build ->
        if
          String_set.is_empty builds_with_deps_set
          || String_set.mem build.Build.name builds_with_deps_set
        then Plan.build plan build)
      x
  in
  Plan.add_dependency_edges plan x;
  let dot = Print.to_dot plan in
  match output with
  | Some path ->
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#cwd / path) dot
  | None -> print_endline dot

type compile_db_format = CompileCommands | CompileFlags

let compile_commands ~path ~builds ~format ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in

  match format with
  | CompileCommands -> (
      let entries = ref [] in
      List.iter
        (fun (b : Build.t) ->
          let source_files = Build.locate_source_files b in
          let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
          let flags = Flags.v () in
          let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in

          List.iter
            (fun (source : Source_file.t) ->
              let ext = Source_file.ext source in
              match Hashtbl.find_opt b.compiler_index ext with
              | None -> ()
              | Some compiler ->
                  let obj =
                    Object_file.of_source ~root:b.source ~build_name:b.name
                      ~build_dir:Eio.Path.(b.build / "obj")
                      source
                  in
                  let file_flags =
                    Hashtbl.find_opt b.compiler_flags ext
                    |> Option.value ~default:(Flags.v ())
                  in
                  let final_flags = Flags.concat b_flags file_flags in
                  let command =
                    compiler.Compiler.command ~flags:final_flags ~output:obj
                  in
                  let entry =
                    `O
                      [
                        ("directory", `String (Eio.Path.native_exn env#cwd));
                        ("file", `String (Eio.Path.native_exn source.path));
                        ( "command",
                          `String
                            (String.concat " "
                               (List.map
                                  (fun s ->
                                    if String.contains s ' ' then
                                      Fmt.str "\"%s\"" s
                                    else s)
                                  command)) );
                        ("output", `String (Eio.Path.native_exn obj.path));
                      ]
                  in
                  entries := entry :: !entries)
            source_files)
        targets;

      let json = `A (List.rev !entries) in
      let contents = Ezjsonm.value_to_string ~minify:false json in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_endline contents)
  | CompileFlags -> (
      let flag_set = ref String_set.empty in
      List.iter
        (fun (b : Build.t) ->
          let pkg = Pkg_config.flags ~env:b.env b.pkgconf in
          let flags = Flags.v () in
          let b_flags = Flags.concat flags @@ Flags.concat pkg b.flags in
          let source_files = Build.locate_source_files b in

          List.iter
            (fun (source : Source_file.t) ->
              let ext = Source_file.ext source in
              match Hashtbl.find_opt b.compiler_index ext with
              | None -> ()
              | Some _ ->
                  let file_flags =
                    Hashtbl.find_opt b.compiler_flags ext
                    |> Option.value ~default:(Flags.v ())
                  in
                  let final_flags = Flags.concat b_flags file_flags in
                  List.iter
                    (fun flag -> flag_set := String_set.add flag !flag_set)
                    final_flags.compile)
            source_files)
        targets;

      let contents =
        String_set.to_list !flag_set |> String.concat "\n" |> fun s -> s ^ "\n"
      in
      match output with
      | Some path ->
          Eio.Path.save ~create:(`Or_truncate 0o644)
            Eio.Path.(env#cwd / path)
            contents
      | None -> print_string contents)
