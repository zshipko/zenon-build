open Types
open Flags
module Build = Build
module Util = Util

module Compiler_config = struct
  type t = {
    name : string;
    ext : string list; [@default []]
    command : string list option; [@default None]
    link_type : Linker.link_type; [@default Linker.Executable]
  }
  [@@deriving yaml]

  let clang =
    { name = "clang"; ext = []; command = None; link_type = Linker.Executable }

  let ispc =
    { name = "ispc"; ext = []; command = None; link_type = Linker.Executable }

  let clangxx =
    {
      name = "clang++";
      ext = [];
      command = None;
      link_type = Linker.Executable;
    }

  let ghc =
    { name = "ghc"; ext = []; command = None; link_type = Linker.Executable }

  let compiler compilers t =
    match t.command with
    | Some cmd ->
        Compiler.
          {
            name = t.name;
            ext = String_set.of_list t.ext;
            command =
              (fun ~flags ~sources:_ ~output ->
                List.fold_left
                  (fun (acc : string list) x ->
                    if String.equal x "#output" then
                      acc @ [ Eio.Path.native_exn output.path ]
                    else if String.equal x "#flags" then acc @ flags.compile
                    else acc @ [ x ])
                  [] cmd);
          }
    | None -> (
        match Compiler.find_by_name compilers t.name with
        | None -> invalid_arg ("unknown compiler: " ^ t.name)
        | Some x -> x)

  let linker linkers t =
    match t.command with
    | Some cmd ->
        Linker.
          {
            name = t.name;
            link_type = t.link_type;
            command =
              (fun ~flags ~objs ~output ->
                List.fold_left
                  (fun (acc : string list) x ->
                    if String.equal x "#objs" then
                      let objs =
                        List.map
                          (fun obj -> Eio.Path.native_exn obj.Object_file.path)
                          objs
                      in
                      acc @ objs
                    else if String.equal x "#output" then
                      acc @ [ Eio.Path.native_exn output ]
                    else if String.equal x "#flags" then acc @ flags.link
                    else acc @ [ x ])
                  [] cmd);
          }
    | None -> (
        match Linker.find_by_name linkers t.name with
        | None -> invalid_arg ("unknown linker: " ^ t.name)
        | Some x -> x)
end

module Build_config = struct
  type flags = {
    compile : string list; [@default []]
    link : string list; [@default []]
  }
  [@@deriving yaml]

  module Lang_flags = struct
    type t = {
      lang : string;
      compile : string list; [@default []]
      link : string list; [@default []]
    }
    [@@deriving yaml]
  end

  let default_compilers =
    [
      Compiler_config.clang;
      Compiler_config.clangxx;
      Compiler_config.ispc;
      Compiler_config.ghc;
    ]

  let default_linkers =
    [
      Compiler_config.clang;
      Compiler_config.clangxx;
      Compiler_config.{ clang with name = "ar" };
      Compiler_config.ghc;
    ]

  type t = {
    name : string option; [@default None]
    root : string option;
    target : string option; [@default None]
    compilers : Compiler_config.t list; [@default default_compilers]
    linker : string option; [@default None]
    files : string list; [@default []]
    ignore : string list; [@default []]
    flags : Lang_flags.t list; [@default []]
    script : string option; [@default None]
    after : string option; [@default None]
    depends_on : string list; [@default []] [@key "depends-on"]
    disable_cache : bool; [@default false]
    only_if : string option; [@default None] [@key "if"]
    pkgconf : string list; [@default []] [@key "pkg"]
    hidden : bool; [@default false]
  }
  [@@deriving yaml]

  let default =
    {
      name = None;
      target = Some "a.out";
      root = Some ".";
      ignore = [];
      compilers = default_compilers;
      linker = None;
      files = [];
      script = None;
      after = None;
      depends_on = [];
      flags = [];
      disable_cache = false;
      only_if = None;
      pkgconf = [];
      hidden = false;
    }
end

type t = {
  build : Build_config.t list;
  flags : Build_config.Lang_flags.t list; [@default []]
  compilers : Compiler_config.t list; [@default Build_config.default_compilers]
  linkers : Compiler_config.t list; [@default Build_config.default_linkers]
  files : string list; [@default []]
  ignore : string list; [@default []]
  pkgconf : string list; [@default []] [@key "pkg"]
}
[@@deriving yaml]

let empty =
  {
    build = [];
    flags = [];
    compilers = [];
    linkers = [];
    files = [];
    ignore = [];
    pkgconf = [];
  }

let read_file path =
  try
    let s = Eio.Path.load path in
    let y = Yaml.of_string_exn s in
    let st = Eio.Path.stat ~follow:true path in
    Result.map (fun y -> (y, st.Eio.File.Stat.mtime)) @@ of_yaml y
  with exn -> Error (`Msg (Printexc.to_string exn))

let rec read_file_or_default path =
  if Eio.Path.is_file path then read_file path
  else if Eio.Path.is_directory path then
    read_file_or_default Eio.Path.(path / "zenon.yaml")
  else Ok (empty, Unix.gettimeofday ())

let init ?mtime ~env path t =
  List.filter_map
    (fun config ->
      let ok =
        match config.Build_config.only_if with
        | Some script -> (
            try
              Eio.Process.run env#process_mgr [ "sh"; "-c"; script ];
              true
            with _ -> false)
        | None -> true
      in
      if not ok then
        let () =
          Util.log "! SKIP %s"
            (Option.value
               ~default:(Option.value ~default:"default" config.root)
               config.name)
        in
        None
      else
        let compilers = List.map (Compiler_config.compiler []) t.compilers in
        let compilers =
          compilers
          @ List.map (Compiler_config.compiler compilers) config.compilers
        in
        let linker_name =
          match config.Build_config.linker with
          | Some linker -> linker (* Specified, so use it *)
          | None -> (
              (* Not specified, use default logic *)
              match config.target with
              | Some target
                when String.starts_with ~prefix:"lib" (Filename.basename target)
                     && String.ends_with ~suffix:".a" (Filename.basename target)
                ->
                  "ar"
              | _ -> Compiler_config.clang.name)
        in
        let linker =
          Compiler_config.linker
            (List.map (Compiler_config.linker []) t.linkers)
            Compiler_config.
              {
                name = linker_name;
                ext = [];
                link_type = Linker.Executable;
                command = None;
              }
        in
        let compiler_flags =
          List.to_seq (t.flags @ config.flags)
          |> Seq.map (fun v ->
                 ( v.Build_config.Lang_flags.lang,
                   Flags.v ~compile:v.compile ~link:v.link () ))
          |> List.of_seq
        in
        let source =
          match config.root with None -> path | Some p -> Eio.Path.(path / p)
        in
        let output =
          Option.map (fun output -> Eio.Path.(env#fs / output)) config.target
        in
        let name =
          match config.name with
          | Some name -> name
          | None -> (
              match config.target with Some p -> p | None -> "default")
        in
        let build =
          Build.v ?script:config.script
            ~pkgconf:(t.pkgconf @ config.pkgconf)
            ?after:config.after ~depends_on:config.depends_on ~linker ~compilers
            ~compiler_flags ?output ~source ~files:(t.files @ config.files)
            ~name ~ignore:(t.ignore @ config.ignore) ~hidden:config.hidden
            ?mtime env
        in
        Some build)
    t.build

let load ~env path =
  match read_file_or_default path with
  | Ok (config, mtime) -> Ok (init ~mtime ~env path config)
  | Error e -> Error e
