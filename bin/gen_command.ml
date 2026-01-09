open Zenon_build
open Common

let pkg ~path ~prefix ~target ~version ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Quiet ~builds:[ target ] env path in
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
  let x = load_config ~log_level:`Quiet ~builds env path in
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

let gitignore ~path ~builds ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Quiet ~builds env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in

  let entries =
    List.fold_left
      (fun acc (b : Build.t) ->
        match b.output with
        | None -> acc
        | Some output ->
            let output_path = Eio.Path.native_exn output in
            (* Make path relative to cwd *)
            let cwd = Eio.Path.native_exn env#cwd in
            let rel_output =
              if String.starts_with ~prefix:cwd output_path then
                let prefix_len = String.length cwd in
                String.sub output_path (prefix_len + 1)
                  (String.length output_path - prefix_len - 1)
              else output_path
            in
            String_set.add rel_output acc)
      String_set.empty targets
  in

  let contents =
    String_set.to_list entries |> List.sort String.compare |> String.concat "\n"
    |> fun s -> s ^ "\n"
  in

  match output with
  | Some path ->
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(env#cwd / path)
        contents
  | None -> print_string contents

let compile_commands ~path ~builds ~format ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Quiet ~builds env path in
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
          let objects = Queue.create () in
          Seq.iter
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
                  Queue.push obj objects;
                  let file_flags =
                    Hashtbl.find_opt b.compiler_flags ext
                    |> Option.value ~default:(Flags.v ())
                  in
                  let final_flags = Flags.concat b_flags file_flags in
                  let command =
                    compiler.Compiler.command ~objects:(Queue.to_seq objects)
                      ~flags:final_flags ~output:obj
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

          Seq.iter
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

let merlin ~path ~builds ?output () =
  Eio_posix.run @@ fun env ->
  let x = load_config ~log_level:`Quiet ~builds env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in
  let ocaml_extensions = String_set.of_list [ "ml"; "mli" ] in
  let source_dirs, build_dirs, ocaml_flags, pkgs =
    List.fold_left
      (fun (source_dirs, ocaml_flags, build_dirs, pkgs) (b : Build.t) ->
        let files = Build.locate_source_files b in
        let source_dirs, ocaml_flags, pkgs =
          Seq.fold_left
            (fun (dirs', flags', pkgs') (source : Source_file.t) ->
              let dir = Filename.dirname @@ Eio.Path.native_exn source.path in
              let dirs' = String_set.add dir dirs' in
              let ext = Source_file.ext source in
              let pkgs = String_set.union (String_set.of_list b.pkgconf) pkgs in
              if String_set.mem ext ocaml_extensions then
                match Hashtbl.find_opt b.compiler_flags ext with
                | Some flags ->
                    ( dirs',
                      String_set.union flags' (String_set.of_list flags.compile),
                      pkgs )
                | None -> (dirs', flags', pkgs')
              else (dirs', flags', pkgs'))
            (source_dirs, ocaml_flags, pkgs)
            files
        in
        let build_dir =
          Eio.Path.native_exn Eio.Path.(b.build / "obj" / b.name)
        in
        (source_dirs, ocaml_flags, String_set.add build_dir build_dirs, pkgs))
      (String_set.empty, String_set.empty, String_set.empty, String_set.empty)
      targets
  in
  let buffer = Buffer.create 1024 in
  let add_line prefix s =
    Buffer.add_string buffer @@ prefix ^ " ";
    Buffer.add_string buffer s;
    Buffer.add_char buffer '\n'
  in
  List.iter (add_line "S") (String_set.to_list source_dirs);
  List.iter (add_line "B") (String_set.to_list build_dirs);
  List.iter (add_line "FLG") (String_set.to_list ocaml_flags);
  List.iter (add_line "PKG") (String_set.to_list pkgs);
  let contents = Buffer.contents buffer in
  match output with
  | Some path ->
      Eio.Path.save ~create:(`Or_truncate 0o644)
        Eio.Path.(env#cwd / path)
        contents
  | None -> print_string contents
