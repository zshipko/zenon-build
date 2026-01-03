open Types
open Compiler_set

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
  mutable flags : Flags.t;
  mutable compiler_flags : (string, Flags.t) Hashtbl.t;
  mtime : float;
}

let add_compile_flags t = Flags.add_compile_flags t.flags
let add_link_flags t = Flags.add_link_flags t.flags
let obj_path t = Eio.Path.(t.build / "obj" / t.name)

let v ?build ?(hidden = false) ?mtime ?(pkgconf = []) ?script ?after
    ?(depends_on = []) ?flags ?(linker = Linker.clang) ?compilers
    ?(compiler_flags = []) ?(files = []) ?(ignore = []) ?(disable_cache = false)
    ?output ~source ~name env =
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
    pkgconf;
    env;
    source;
    build;
    compiler_index;
    compilers;
    linker;
    files = List.map Util.glob_path files;
    script;
    after;
    depends_on;
    flags = Option.value ~default:(Flags.v ()) flags;
    output;
    ignore = List.map Util.glob_path ignore;
    name;
    compiler_flags;
    disable_cache;
    mtime = Option.value ~default:(Unix.gettimeofday ()) mtime;
    hidden;
  }

let locate_source_files t : Source_file.t list =
  let re = Re.alt t.files |> Re.compile in
  let ignore = Re.alt t.ignore |> Re.compile in
  let check_ignore f =
    match t.ignore with [] -> true | _ -> not (Re.execp ignore f)
  in
  let is_special_dir name = List.mem name [ "zenon-build"; ".git"; ".jj" ] in

  let rec inner path =
    let entries = Eio.Path.read_dir path in
    List.concat_map
      (fun name ->
        let full_path = Eio.Path.(path / name) in
        if Eio.Path.is_directory full_path then
          if check_ignore name && not (is_special_dir name) then inner full_path
          else []
        else if Re.execp re (Eio.Path.native_exn full_path) then
          [ Source_file.v ~root:t.source full_path ]
        else [])
      entries
  in
  inner t.source

let parse_compile_flags f =
  Eio.Path.with_lines f @@ fun lines -> Seq.map String.trim lines |> List.of_seq

let compile_flags t =
  let f = Eio.Path.(t.source / "compile_flags.txt") in
  if Eio.Path.is_file f then parse_compile_flags f else []

let add_source_file t path = t.files <- t.files @ [ Util.glob_path path ]

let add_source_files t ?(reset = false) files =
  if reset then t.files <- [];
  List.iter (fun f -> add_source_file t f) files

let clean t = Eio.Path.rmtree ~missing_ok:true t.build
let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)
