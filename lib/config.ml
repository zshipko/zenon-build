open Types
open Flags
module Build = Build
module Util = Util

module Compiler_config = struct
  type t = {
    name : string;
    ext : string list; [@default []]
    command : string list option; [@default None]
    link_type : string; [@key "link-type"] [@default "exe"]
  }
  [@@deriving yaml]

  let clang = { name = "clang"; ext = []; command = None; link_type = "exe" }

  let flang =
    { name = "flang-new"; ext = []; command = None; link_type = "exe" }

  let ispc = { name = "ispc"; ext = []; command = None; link_type = "exe" }

  let clangxx =
    { name = "clang++"; ext = []; command = None; link_type = "exe" }

  let ghc = { name = "ghc"; ext = []; command = None; link_type = "exe" }
  let mlton = { name = "mlton"; ext = []; command = None; link_type = "exe" }
  let ats2 = { name = "patscc"; ext = []; command = None; link_type = "exe" }

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
            link_type = Linker.link_type_of_string t.link_type;
            exts = String_set.of_list t.ext;
            has_runtime = false;
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

  type t = {
    name : string option; [@default None]
    root : string option;
    target : string option; [@default None]
    compilers : string list; [@default []]
    linker : string option; [@default None]
    files : string list; [@default []]
    headers : string list; [@default []]
    ignore : string list; [@default []]
    flags : Lang_flags.t list; [@default []]
    script : string option; [@default None]
    after : string option; [@default None]
    depends_on : string list; [@default []] [@key "depends-on"]
    disable_cache : bool; [@default false]
    only_if : string option; [@default None] [@key "if"]
    pkgconf : string list; [@default []] [@key "pkg"]
    hidden : bool; [@default false]
    parallel : bool; [@default true]
  }
  [@@deriving yaml]

  let default =
    {
      name = None;
      target = Some "a.out";
      root = Some ".";
      ignore = [];
      compilers = [];
      linker = None;
      files = [];
      headers = [];
      script = None;
      after = None;
      depends_on = [];
      flags = [];
      disable_cache = false;
      only_if = None;
      pkgconf = [];
      hidden = false;
      parallel = true;
    }
end

let default_compilers =
  [
    Compiler_config.clang;
    Compiler_config.clangxx;
    Compiler_config.flang;
    Compiler_config.ispc;
    Compiler_config.ghc;
    Compiler_config.mlton;
    Compiler_config.ats2;
  ]

let default_linkers =
  [
    Compiler_config.clang;
    Compiler_config.clangxx;
    Compiler_config.flang;
    Compiler_config.{ clang with name = "ar" };
    Compiler_config.ghc;
    Compiler_config.mlton;
    Compiler_config.ats2;
  ]

module Tools = struct
  type t = {
    compilers : Compiler_config.t list; [@default default_compilers]
    linkers : Compiler_config.t list; [@default default_linkers]
  }
  [@@deriving yaml]

  let default = { compilers = default_compilers; linkers = default_linkers }
  let empty = { compilers = []; linkers = [] }
end

type t = {
  build : Build_config.t list;
  flags : Build_config.Lang_flags.t list; [@default []]
  tools : Tools.t; [@default Tools.default]
  files : string list; [@default []]
  ignore : string list; [@default []]
  pkgconf : string list; [@default []] [@key "pkg"]
}
[@@deriving yaml]

let empty =
  {
    build = [];
    flags = [];
    tools = Tools.empty;
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
  (* Register custom compilers and linkers globally *)
  let global_compilers =
    List.map (Compiler_config.compiler []) t.tools.compilers
  in
  let global_linkers = List.map (Compiler_config.linker []) t.tools.linkers in
  List.iter Compiler.register global_compilers;
  List.iter Linker.register global_linkers;

  (* Parse .gitignore from the root path - returns Re.t list *)
  let gitignore_patterns =
    Util.parse_gitignore Eio.Path.(path / ".gitignore")
  in
  (* Config ignore patterns are strings that need to be converted *)
  let config_ignore_patterns = List.map Util.glob_path t.ignore in

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
        let all_compilers = !Compiler.all in
        let compilers =
          if List.is_empty config.compilers then global_compilers
          else
            List.filter_map
              (fun name -> Compiler.find_by_name all_compilers name)
              config.compilers
        in
        let linker_name, link_type =
          match config.Build_config.linker with
          | Some linker -> (linker, "exe") (* User-specified linker *)
          | None -> (
              match config.target with
              | Some target when Util.is_static_lib (Filename.basename target)
                ->
                  ("ar", "static")
              | Some target when Util.is_shared_lib (Filename.basename target)
                ->
                  let has_cxx =
                    List.exists
                      (fun name -> name = "clang++" || name = "g++")
                      config.compilers
                  in
                  let linker_name =
                    if has_cxx then "clang++-shared" else "clang-shared"
                  in
                  (linker_name, "shared")
              | _ -> (Compiler_config.clang.name, "exe"))
        in
        let linker =
          Compiler_config.linker !Linker.all
            Compiler_config.
              { name = linker_name; ext = []; link_type; command = None }
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
          Option.map
            (fun output ->
              let normalized = Util.normalize_shared_lib_ext output in
              Eio.Path.(env#fs / normalized))
            config.target
        in
        let name =
          match config.name with
          | Some name -> name
          | None -> (
              match config.target with Some p -> p | None -> "default")
        in
        (* Combine gitignore patterns (Re.t) with config ignore patterns (strings) *)
        let build_ignore_patterns = List.map Util.glob_path config.ignore in
        let all_ignore =
          gitignore_patterns @ config_ignore_patterns @ build_ignore_patterns
        in
        let build =
          Build.v ~parallel:config.parallel ?script:config.script
            ~pkgconf:(t.pkgconf @ config.pkgconf)
            ?after:config.after ~depends_on:config.depends_on ~linker ~compilers
            ~compiler_flags ?output ~source ~files:(t.files @ config.files)
            ~headers:config.headers ~name ~ignore:all_ignore
            ~hidden:config.hidden ?mtime env
        in
        Some build)
    t.build

let load ~env path =
  match read_file_or_default path with
  | Ok (config, mtime) -> Ok (init ~mtime ~env path config)
  | Error e -> Error e
