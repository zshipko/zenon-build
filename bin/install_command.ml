open Zenon
open Common

let install ~path ~builds ~prefix ~version ~uninstall () =
  Eio_posix.run @@ fun env ->
  let x = load_config env path in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
  let targets =
    List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x
  in
  let make_install_path subdir = Eio.Path.(env#fs / prefix / subdir) in

  if uninstall then
    (* Uninstall mode *)
    List.iter
      (fun (b : Build.t) ->
        (* Uninstall the built artifact *)
        (match b.output with
        | None -> ()
        | Some output_path -> (
            let sources = Build.locate_source_files b in
            let linker =
              Linker.auto_select_linker ~sources ~linker:b.linker b.name
            in
            let install_dir =
              match linker.link_type with
              | Linker.Executable -> make_install_path "bin"
              | Linker.Shared | Linker.Static -> make_install_path "lib"
            in
            let filename =
              Filename.basename (Eio.Path.native_exn output_path)
            in
            let dest = Eio.Path.(install_dir / filename) in

            (* Remove the file *)
            (try
               Eio.Path.unlink dest;
               Util.log "UNINSTALL %s" (Eio.Path.native_exn dest)
             with Eio.Io _ ->
               Util.log "WARNING: File %s not found, skipping"
                 (Eio.Path.native_exn dest));

            (* Uninstall pkg-config file for libraries *)
            match linker.link_type with
            | Linker.Shared | Linker.Static -> (
                let lib_name = lib_name b in
                let pc_dir = make_install_path "lib/pkgconfig" in
                let pc_file = Eio.Path.(pc_dir / (lib_name ^ ".pc")) in
                try
                  Eio.Path.unlink pc_file;
                  Util.log "UNINSTALL %s" (Eio.Path.native_exn pc_file)
                with Eio.Io _ ->
                  Util.log "WARNING: File %s not found, skipping"
                    (Eio.Path.native_exn pc_file))
            | Linker.Executable -> ()));

        (* Uninstall header files *)
        let headers = Build.locate_headers b in
        if not (List.is_empty headers) then (
          let include_name = lib_name b in
          let include_dir = make_install_path ("include/" ^ include_name) in
          List.iter
            (fun header_path ->
              let rel_path = Util.relative_to b.source header_path in
              let dest = Eio.Path.(include_dir / rel_path) in
              try
                Eio.Path.unlink dest;
                Util.log "UNINSTALL %s" (Eio.Path.native_exn dest)
              with Eio.Io _ ->
                Util.log "WARNING: File %s not found, skipping"
                  (Eio.Path.native_exn dest))
            headers;
          (* Try to remove the include directory if it's empty *)
          try Eio.Path.rmdir include_dir with Eio.Io _ -> ()))
      targets
  else
    (* Install mode *)
    List.iter
      (fun (b : Build.t) ->
        (* Install the built artifact *)
        (match b.output with
        | None -> Util.log "Skipping %s (no output)" b.name
        | Some output_path -> (
            (* Check if output file exists *)
            match Eio.Path.kind ~follow:true output_path with
            | `Regular_file -> (
                let sources = Build.locate_source_files b in
                let linker =
                  Linker.auto_select_linker ~sources ~linker:b.linker b.name
                in
                let install_dir =
                  match linker.link_type with
                  | Linker.Executable -> make_install_path "bin"
                  | Linker.Shared | Linker.Static -> make_install_path "lib"
                in
                Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 install_dir;
                let filename =
                  Filename.basename (Eio.Path.native_exn output_path)
                in
                let dest = Eio.Path.(install_dir / filename) in

                (* Copy the file *)
                Util.log "INSTALL %s -> %s"
                  (Eio.Path.native_exn output_path)
                  (Eio.Path.native_exn dest);
                let contents = Eio.Path.load output_path in
                Eio.Path.save ~create:(`Or_truncate 0o755) dest contents;

                (* Install pkg-config file for libraries *)
                match linker.link_type with
                | Linker.Shared | Linker.Static ->
                    let lib_name = lib_name b in
                    let c_flags = c_flags b in
                    let flags = Flags.concat b.flags c_flags in
                    let pc_contents =
                      Pkg_config.generate ~lib_name ~prefix ~version
                        ~requires:b.pkgconf ~cflags:flags.compile
                        ~ldflags:flags.link b.name
                    in
                    let pc_dir = make_install_path "lib/pkgconfig" in
                    Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true pc_dir;
                    let pc_file = Eio.Path.(pc_dir / (lib_name ^ ".pc")) in
                    Util.log "INSTALL pkg-config -> %s"
                      (Eio.Path.native_exn pc_file);
                    Eio.Path.save ~create:(`Or_truncate 0o644) pc_file
                      pc_contents
                | Linker.Executable -> ())
            | _ ->
                Util.log "WARNING: Output file %s not found, skipping"
                  (Eio.Path.native_exn output_path)
            | exception Eio.Io _ ->
                Util.log "WARNING: Output file %s not found, skipping"
                  (Eio.Path.native_exn output_path)));

        (* Install header files *)
        let headers = Build.locate_headers b in
        if not (List.is_empty headers) then (
          let include_name = lib_name b in
          let include_dir = make_install_path ("include/" ^ include_name) in
          Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true include_dir;
          List.iter
            (fun header_path ->
              let rel_path = Util.relative_to b.source header_path in
              let dest = Eio.Path.(include_dir / rel_path) in
              let dest_dir =
                Eio.Path.(include_dir / Filename.dirname rel_path)
              in

              Eio.Path.mkdirs ~perm:0o755 ~exists_ok:true dest_dir;

              Util.log "INSTALL %s -> %s"
                (Eio.Path.native_exn header_path)
                (Eio.Path.native_exn dest);
              let contents = Eio.Path.load header_path in
              Eio.Path.save ~create:(`Or_truncate 0o644) dest contents)
            headers))
      targets
