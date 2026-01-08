open Zenon
open Common

let build ?output ?(ignore = []) ~arg ~cflags ~ldflags ~path ~builds ~file ~run
    ~pkg ~(linker : string option) ~log_level () =
  Eio_posix.run @@ fun env ->
  let ignore_patterns = List.map Util.glob ignore in
  let x = load_config ~log_level ~builds env path in
  let builds, x =
    match x with
    | [] ->
        ( [ "default" ],
          [
            Build.v env ~ignore:ignore_patterns ~pkgconf:pkg ~flags:(Flags.v ())
              ~source:Eio.Path.(env#fs / path)
              ~files:file ~name:"default"
              ?output:(Option.map (fun x -> Eio.Path.(env#cwd / x)) output)
              ~log_level
              ?linker:
                (Option.map
                   (fun name -> Config.Compiler_config.(linker (named name)))
                   linker);
          ] )
    | x -> (builds, x)
  in
  let builds = filter_builds x builds in
  let build_map = make_build_map x in
  let builds_with_deps_set = builds_with_deps build_map builds in
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
      let flags = try Hashtbl.find cflags_by_lang lang with Not_found -> [] in
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
  let () =
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
                Option.map
                  (fun l -> Config.Compiler_config.(linker @@ named l))
                  linker
                |> Option.value ~default:build.Build.linker;
            }))
      x
  in
  Plan.run_all ~execute:run ~args:arg ~log_level ~env plan
    (List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x)
