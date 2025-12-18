open Types
open Compiler

type t = {
  env : Eio_posix.stdenv;
  source : path;
  build : path;
  compiler_index : (string, Compiler.t) Hashtbl.t;
  compilers : Compiler_set.t;
  linker : Compiler.t;
  script : string option;
  after : string option;
  name : string;
  mutable pkgconf : string list;
  mutable ignore : Re.t list;
  mutable disable_cache : bool;
  mutable output : path option;
  mutable files : Re.t list;
  mutable flags : Flags.t;
  mutable compiler_flags : (string, Flags.t) Hashtbl.t;
}

let add_compile_flags t = Flags.add_compile_flags t.flags
let add_link_flags t = Flags.add_link_flags t.flags
let obj_path t = Eio.Path.(t.build / "obj")

let v ?build ?(pkgconf = []) ?script ?after ?flags ?(linker = Compiler.clang)
    ?compilers ?(compiler_flags = []) ?(files = []) ?(ignore = [])
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
      String_set.iter
        (fun ext -> Hashtbl.replace compiler_index ext c)
        c.Compiler.ext)
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
    flags = Option.value ~default:(Flags.v ()) flags;
    output;
    ignore = List.map Util.glob_path ignore;
    name;
    compiler_flags;
    disable_cache;
  }

let locate_source_files t : Source_file.t list =
  let re = Re.alt t.files |> Re.compile in
  let rec inner path =
    let files = Eio.Path.read_dir path in
    List.fold_left
      (fun (acc : Source_file.t list) file ->
        let f = Eio.Path.(path / file) in
        if Eio.Path.is_directory f then
          if
            not
              (String.equal file "zenon-build"
              || String.equal file ".git" || String.equal file ".jj")
          then inner f @ acc
          else acc
        else if Re.execp re (Eio.Path.native_exn f) then
          Source_file.v ~root:t.source f :: acc
        else acc)
      [] files
  in
  inner t.source

let parse_compile_flags t f =
  Eio.Path.with_lines f @@ fun lines ->
  let lines = Seq.map String.trim lines |> List.of_seq in
  add_compile_flags t lines

let flags_from_compile_flags t =
  let f = Eio.Path.(t.source / "compile_flags.txt") in
  if Eio.Path.is_file f then parse_compile_flags t f
  else if Eio.Path.is_file Eio.Path.(t.env#cwd / "compile_flags.txt") then
    parse_compile_flags t Eio.Path.(t.env#cwd / "compile_flags.txt")

let add_source_file t path = t.files <- t.files @ [ Util.glob_path path ]
let add_source_files t files = List.iter (fun f -> add_source_file t f) files
let clean t = Eio.Path.rmtree ~missing_ok:true t.build
let clean_obj t = Eio.Path.rmtree ~missing_ok:true (obj_path t)
