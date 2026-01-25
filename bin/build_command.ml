open Zenon_build
open Common

let build ?output ?(ignore = []) ~arg ~cflags ~ldflags ~path ~builds ~file ~run
    ~pkg ~(linker : string option) ~log_level ~watch () =
  Eio_posix.run @@ fun env ->
  let get_config_path () =
    let eio_path = Eio.Path.(env#fs / Unix.realpath path) in
    let dir_path =
      if Eio.Path.is_directory eio_path then eio_path
      else
        match Eio.Path.split eio_path with Some (d, _) -> d | None -> eio_path
    in
    Config.find_config_in_parents dir_path
  in

  let get_file_mtimes build_configs =
    let all_files = ref String_map.empty in
    (* Add config file mtime *)
    (match get_config_path () with
    | Some config_path ->
        let path_str = Eio.Path.native_exn config_path in
        let mtime = (Eio.Path.stat ~follow:true config_path).mtime in
        all_files := String_map.add path_str mtime !all_files
    | None -> ());
    List.iter
      (fun (build : Build.t) ->
        let sources = Build.locate_source_files build |> List.of_seq in
        List.iter
          (fun (sf : Source_file.t) ->
            let path = Eio.Path.native_exn sf.path in
            try
              let mtime = (Eio.Path.stat ~follow:true sf.path).mtime in
              all_files := String_map.add path mtime !all_files
            with _ ->
              (* File might have been deleted during the build process,
                 which is fine. *)
              ())
          sources)
      build_configs;
    !all_files
  in

  let do_build build_configs =
    let ignore_patterns = List.map Util.glob ignore in
    let targets, current_build_configs =
      match build_configs with
      | [] ->
          ( [ "default" ],
            [
              Build.v env ~ignore:ignore_patterns ~pkgconf:pkg
                ~flags:(Flags.v ())
                ~source:Eio.Path.(env#fs / path)
                ~files:file ~name:"default"
                ?output:(Option.map (fun x -> Eio.Path.(env#cwd / x)) output)
                ~log_level
                ?linker:
                  (Option.map
                     (fun name -> Config.Compiler_config.(linker (named name)))
                     linker);
            ] )
      | configs -> (builds, configs)
    in
    let active_targets = filter_builds current_build_configs targets in
    let build_map = make_build_map current_build_configs in
    let builds_with_deps_set = builds_with_deps build_map active_targets in
    let plan = Plan.v () in
    (* Separate generic and language-specific flags *)
    let generic_cflags, lang_cflags =
      List.partition (fun (lang, _) -> lang = "all") cflags
    in
    let generic_ldflags, lang_ldflags =
      List.partition (fun (lang, _) -> lang = "all") ldflags
    in
    let generic_cflags = List.map snd generic_cflags in
    let generic_ldflags = List.map snd generic_ldflags in
    let cflags_by_lang = Hashtbl.create 8 in
    List.iter
      (fun (lang, flag) ->
        let flags =
          try Hashtbl.find cflags_by_lang lang with Not_found -> []
        in
        Hashtbl.replace cflags_by_lang lang (flag :: flags))
      lang_cflags;
    let ldflags_by_lang = Hashtbl.create 8 in
    List.iter
      (fun (lang, flag) ->
        let flags =
          try Hashtbl.find ldflags_by_lang lang with Not_found -> []
        in
        Hashtbl.replace ldflags_by_lang lang (flag :: flags))
      lang_ldflags;
    List.iter
      (fun build ->
        if
          String_set.is_empty builds_with_deps_set
          || String_set.mem build.Build.name builds_with_deps_set
        then (
          let output =
            match output with
            | None -> build.Build.output
            | Some output -> Some Eio.Path.(env#cwd / output)
          in
          (* Add generic flags to build.flags *)
          Flags.add_compile_flags build.Build.flags generic_cflags;
          Flags.add_link_flags build.Build.flags generic_ldflags;
          (* Add language-specific flags to build.compiler_flags *)
          Hashtbl.iter
            (fun lang flags ->
              let existing_flags =
                try Hashtbl.find build.Build.compiler_flags lang
                with Not_found ->
                  let new_flags = Flags.v () in
                  Hashtbl.add build.Build.compiler_flags lang new_flags;
                  new_flags
              in
              Flags.add_compile_flags existing_flags (List.rev flags))
            cflags_by_lang;
          Hashtbl.iter
            (fun lang flags ->
              let existing_flags =
                try Hashtbl.find build.Build.compiler_flags lang
                with Not_found ->
                  let new_flags = Flags.v () in
                  Hashtbl.add build.Build.compiler_flags lang new_flags;
                  new_flags
              in
              Flags.add_link_flags existing_flags (List.rev flags))
            ldflags_by_lang;
          let () =
            Build.add_source_files ~reset:(not (List.is_empty file)) build file
          in
          Plan.build plan
            {
              build with
              log_level;
              pkgconf = build.Build.pkgconf @ pkg;
              ignore = build.Build.ignore @ ignore_patterns;
              output;
              linker =
                (match linker with
                | Some l -> Some Config.Compiler_config.(linker @@ named l)
                | None -> build.Build.linker);
            }))
      current_build_configs;
    Plan.run_all ~execute:run ~args:arg ~log_level plan
      (List.filter
         (fun b -> String_set.mem b.Build.name builds_with_deps_set)
         current_build_configs);
    get_file_mtimes current_build_configs
  in

  let initial_build_configs = load_config ~log_level ~builds env path in
  let initial_mtimes = do_build initial_build_configs in

  if watch then
    let rec loop mtimes =
      Eio.Time.sleep env#clock 2.0;
      let new_build_configs = load_config ~log_level ~builds env path in
      let new_mtimes = get_file_mtimes new_build_configs in
      if not (String_map.equal Float.equal mtimes new_mtimes) then (
        Util.log
          ~verbose:(Util.is_verbose log_level)
          "+ Change detected, rebuilding...";
        let updated_mtimes = do_build new_build_configs in
        loop updated_mtimes)
      else loop mtimes
    in
    loop initial_mtimes
