open Types

type t = {
  env : Eio_posix.stdenv;
  hidden : bool;
  source : path;
  build : path;
  compiler_index : (string, Compiler.t) Hashtbl.t;
  compilers : Compiler_set.t;
  linker : Linker.t;
  script : string option;
  after : string option;
  depends_on : string list;
  name : string;
  pkgconf : string list;
  ignore : Re.t list;
  output : path option;
  mutable disable_cache : bool;
  mutable files : Re.t list;
  mutable headers : Re.t list;
  mutable flags : Flags.t;
  mutable compiler_flags : (string, Flags.t) Hashtbl.t;
  mtime : float;
  parallel : bool;
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
    | None -> Compiler_set.default
    | Some compilers ->
        Compiler_set.union Compiler_set.default
        @@ Compiler_set.of_list compilers
  in
  let build =
    match build with
    | None -> Eio.Path.(env#cwd / "zenon-build")
    | Some path -> path
  in
  Eio.Path.mkdirs ~exists_ok:true build ~perm:0o755;
  let compiler_flags = Hashtbl.of_seq (List.to_seq compiler_flags) in
  let compiler_index = Hashtbl.create 8 in
  Compiler_set.iter
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
    files = List.map Util.glob_path files;
    headers = List.map Util.glob_path headers;
    script;
    after;
    depends_on;
    flags = Option.value ~default:(Flags.v ()) flags;
    output;
    ignore;
    (* Already Re.t list from config.ml *)
    name;
    compiler_flags;
    disable_cache;
    mtime = Option.value ~default:(Unix.gettimeofday ()) mtime;
    hidden;
  }

let special_dirs = String_set.of_list [ "zenon-build"; ".git"; ".jj" ]
let is_special_dir name = String_set.mem name special_dirs

let locate_files t patterns =
  if List.is_empty patterns then []
  else
    let ignore = Re.alt t.ignore |> Re.compile in
    let check_ignore f =
      match t.ignore with [] -> true | _ -> not (Re.execp ignore f)
    in
    let rec collect_all path =
      let entries = Eio.Path.read_dir path in
      List.concat_map
        (fun name ->
          let full_path = Eio.Path.(path / name) in
          if Eio.Path.is_directory full_path then
            if check_ignore name && not (is_special_dir name) then
              collect_all full_path
            else []
          else [ full_path ])
        entries
    in
    (* Collect all files first *)
    let all_files = collect_all t.source in
    (* Match files in pattern order, preserving order from config *)
    let seen = Hashtbl.create (List.length all_files) in
    List.concat_map
      (fun pattern ->
        let re = Re.compile pattern in
        List.filter_map
          (fun path ->
            let path_str = Eio.Path.native_exn path in
            if Re.execp re path_str && not (Hashtbl.mem seen path_str) then (
              Hashtbl.add seen path_str true;
              Some path)
            else None)
          all_files)
      patterns

let locate_source_files t : Source_file.t list =
  locate_files t t.files
  |> List.map (fun path -> Source_file.v ~root:t.source path)

let locate_headers t : path list = locate_files t t.headers
let add_source_file t path = t.files <- t.files @ [ Util.glob_path path ]

let add_source_files t ?(reset = false) files =
  if reset then t.files <- [];
  List.iter (fun f -> add_source_file t f) files

let clean t = Eio.Path.rmtree ~missing_ok:true t.build
let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)
