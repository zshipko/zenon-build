open Zenon
open Common

let build ?output ?(ignore = []) ~arg ~cflags ~ldflags ~path ~builds ~file ~run
    ~pkg ~(linker : string option) ~log_level () =
  Eio_posix.run @@ fun env ->
  let ignore_patterns = List.map Util.glob_path ignore in
  let x = load_config env path in
  let builds, x =
    match x with
    | [] ->
        ( [ "default" ],
          [
            Build.v env ~ignore:ignore_patterns ~pkgconf:pkg
              ~flags:(Flags.v ~compile:cflags ~link:ldflags ())
              ~source:Eio.Path.(env#fs / path)
              ~files:file ~name:"default"
              ?output:(Option.map (fun x -> Eio.Path.(env#cwd / x)) output)
              ?linker:
                (Option.map
                   (fun name ->
                     Config.Compiler_config.(
                       linker
                         {
                           name;
                           ext = [];
                           command = None;
                           link_type = "exe";
                           has_runtime = false;
                         }))
                   linker);
          ] )
    | x -> (builds, x)
  in
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
        then
          let output =
            match output with
            | None -> build.Build.output
            | Some output -> Some Eio.Path.(env#cwd / output)
          in
          let () = Build.add_compile_flags build cflags in
          let () = Build.add_link_flags build ldflags in
          let () =
            Build.add_source_files ~reset:(not (List.is_empty file)) build file
          in
          Plan.build plan
            {
              build with
              pkgconf = build.Build.pkgconf @ pkg;
              ignore = build.Build.ignore @ ignore_patterns;
              output;
              linker =
                Option.map
                  (fun name ->
                    Config.Compiler_config.(
                      linker
                        {
                          name;
                          ext = [];
                          command = None;
                          link_type = "exe";
                          has_runtime = false;
                        }))
                  linker
                |> Option.value ~default:build.Build.linker;
            })
      x
  in
  Plan.run_all ~execute:run ~args:arg ~log_level ~env plan
    (List.filter (fun b -> String_set.mem b.Build.name builds_with_deps_set) x)
