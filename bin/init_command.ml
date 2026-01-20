open Zenon_build

let string s = `String s

let init ~path ~log_level ~file ~ignore ~ldflag ~cflag ~pkg ~linker ~target =
  Eio_posix.run @@ fun env ->
  let cwd = Eio.Path.(env#fs / path) in
  if not (Eio.Path.is_directory cwd) then (
    Util.log ~verbose:(Util.is_verbose log_level) "Creating directory `%s`" path;
    Eio.Path.mkdir ~perm:0o755 cwd);
  let zenon_file = Eio.Path.(cwd / "zenon.yaml") in
  let p = Eio.Path.native_exn zenon_file in
  if Sys.file_exists p then
    Util.log ~verbose:true "Warning: `%s` already exists" p
  else
    let build_config = ref [ ("files", `A (List.map string file)) ] in
    if not (List.is_empty ignore) then
      build_config := ("ignore", `A (List.map string ignore)) :: !build_config;
    let flags_tbl = Hashtbl.create 8 in
    List.iter
      (fun (lang, flag) ->
        let compile, link =
          Option.value (Hashtbl.find_opt flags_tbl lang) ~default:([], [])
        in
        Hashtbl.replace flags_tbl lang (flag :: compile, link))
      cflag;
    List.iter
      (fun (lang, flag) ->
        let compile, link =
          Option.value (Hashtbl.find_opt flags_tbl lang) ~default:([], [])
        in
        Hashtbl.replace flags_tbl lang (compile, flag :: link))
      ldflag;
    if Hashtbl.length flags_tbl > 0 then (
      let flags_yaml =
        Hashtbl.to_seq flags_tbl
        |> Seq.map (fun (lang, (compile, link)) ->
            `O
              [
                ("lang", `String lang);
                ("compile", `A (List.map string (List.rev compile)));
                ("link", `A (List.map string (List.rev link)));
              ])
        |> List.of_seq
      in
      build_config := ("flags", `A flags_yaml) :: !build_config;
      if not (List.is_empty pkg) then
        build_config := ("pkg", `A (List.map string pkg)) :: !build_config;
      Option.iter
        (fun l -> build_config := ("linker", string l) :: !build_config)
        linker;
      Option.iter
        (fun t -> build_config := ("target", string t) :: !build_config)
        target;
      let contents = `O [ ("build", `A [ `O (List.rev !build_config) ]) ] in
      let contents = Yaml.to_string_exn ~layout_style:`Block contents in
      Eio.Path.save ~create:(`If_missing 0o644) zenon_file contents)
