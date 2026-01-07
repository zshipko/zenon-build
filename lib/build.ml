open Types

type t = {
  env : Eio_posix.stdenv;
  hidden : bool;
  source : path;
  build : path;
  compiler_index : (string, Compiler.t) Hashtbl.t;
  compilers : Compiler.Set.t;
  linker : Linker.t;
  script : string option;
  after : string option;
  depends_on : string list;
  name : string;
  pkgconf : string list;
  ignore : Re.t list;
  output : path option;
  disable_cache : bool;
  mtime : float;
  parallel : bool;
  mutable files : Re.t list;
  mutable headers : Re.t list;
  mutable flags : Flags.t;
  mutable compiler_flags : (string, Flags.t) Hashtbl.t;
}

let add_compile_flags t = Flags.add_compile_flags t.flags
let add_link_flags t = Flags.add_link_flags t.flags
let obj_path t = Eio.Path.(t.build / "obj" / t.name)

let v ?build ?(parallel = true) ?(hidden = false) ?mtime ?(pkgconf = []) ?script
    ?after ?(depends_on = []) ?flags ?(linker = Linker.clang) ?compilers
    ?(compiler_flags = []) ?(files = []) ?(headers = []) ?(ignore = [])
    ?(disable_cache = false) ?output ~source ~name env =
  let compilers =
    match compilers with
    | None -> Compiler.Set.default
    | Some compilers ->
        Compiler.Set.union Compiler.Set.default
        @@ Compiler.Set.of_list compilers
  in
  let build =
    match build with
    | None -> Eio.Path.(env#cwd / "zenon-build")
    | Some path -> path
  in
  Eio.Path.mkdirs ~exists_ok:true build ~perm:0o755;
  let compiler_flags = Hashtbl.of_seq (List.to_seq compiler_flags) in
  let compiler_index = Hashtbl.create 8 in
  Compiler.Set.iter
    (fun c ->
      String_set.iter (fun ext -> Hashtbl.replace compiler_index ext c) c.ext)
    compilers;
  {
    parallel;
    pkgconf;
    env;
    source;
    build;
    compiler_index;
    compilers;
    linker;
    files = List.map Util.glob files;
    headers = List.map Util.glob headers;
    script;
    after;
    depends_on;
    flags = Option.value ~default:(Flags.v ()) flags;
    output;
    ignore;
    name;
    compiler_flags;
    disable_cache;
    mtime = Option.value ~default:(Unix.gettimeofday ()) mtime;
    hidden;
  }

let special_dirs = String_set.of_list [ "zenon-build"; ".git"; ".jj" ]
let is_special_dir name = String_set.mem name special_dirs

let rec collect_all count check_ignore root path =
  let entries = Eio.Path.read_dir path |> List.to_seq in
  Seq.concat_map
    (fun name ->
      incr count;
      let full_path = Eio.Path.(path / name) in
      if Eio.Path.is_directory full_path then
        let rel_path = Util.relative_to root full_path in
        if (not (is_special_dir name)) && check_ignore rel_path then
          collect_all count check_ignore root full_path
        else Seq.empty
      else Seq.return full_path)
    entries

let locate_files t = function
  | [] -> Seq.empty
  | patterns ->
      let ignore = Re.alt t.ignore |> Re.compile in
      let check_ignore f =
        match t.ignore with [] -> true | _ -> not (Re.execp ignore f)
      in
      let count = ref 0 in
      let all_files = collect_all count check_ignore t.source t.source in
      let seen = Hashtbl.create !count in
      let re = Re.alt patterns |> Re.compile in
      Seq.filter_map
        (fun path ->
          let path_str = Eio.Path.native_exn path in
          let rel_path = Util.relative_to t.source path in
          if (not (Hashtbl.mem seen path_str)) && Re.execp re rel_path then (
            Hashtbl.add seen path_str true;
            Some path)
          else None)
        all_files

let locate_source_files t : Source_file.t Seq.t =
  locate_files t t.files
  |> Seq.map (fun path -> Source_file.v ~root:t.source path)

let locate_headers t : path Seq.t = locate_files t t.headers
let add_source_file t path = t.files <- t.files @ [ Util.glob path ]

let add_source_files t ?(reset = false) files =
  if reset then t.files <- [];
  List.iter (fun f -> add_source_file t f) files

let clean t = Eio.Path.rmtree ~missing_ok:true t.build
let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)
