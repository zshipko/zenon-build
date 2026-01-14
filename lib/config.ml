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
    has_runtime : bool; [@key "has-runtime"] [@default false]
    parallel : bool; [@default true]
    compile_flag_prefix : string option;
        [@key "compile-flag-prefix"] [@default None]
    link_flag_prefix : string option; [@key "link-flag-prefix"] [@default None]
  }
  [@@deriving yaml]

  let named name =
    {
      name;
      ext = [];
      command = None;
      link_type = "exe";
      has_runtime = false;
      parallel = true;
      compile_flag_prefix = None;
      link_flag_prefix = None;
    }

  let wrap_c_flags t flags =
    if Option.is_none t.compile_flag_prefix && Option.is_none t.link_flag_prefix
    then flags
    else
      let compile =
        Option.fold ~none:flags.Flags.compile
          ~some:(fun prefix ->
            List.concat_map (fun x -> [ prefix; x ]) flags.compile)
          t.compile_flag_prefix
      in
      let link =
        Option.fold ~none:flags.link
          ~some:(fun prefix ->
            List.concat_map (fun x -> [ prefix; x ]) flags.link)
          t.link_flag_prefix
      in
      Flags.v ~compile ~link ()

  let compiler ?compilers t =
    match t.command with
    | Some cmd ->
        Compiler.
          {
            name = t.name;
            ext = String_set.of_list t.ext;
            command =
              (fun ~flags ~objects:_ ~output ->
                List.concat_map
                  (fun x ->
                    if String.equal x "#output" then
                      [ Eio.Path.native_exn output.path ]
                    else if String.equal x "#flags" then flags.compile
                    else [ x ])
                  cmd);
            transform_output = Fun.id;
            parallel = t.parallel;
            wrap_c_flags = wrap_c_flags t;
          }
    | None -> (
        match Compiler.find_by_name ?compilers t.name with
        | None -> invalid_arg ("unknown compiler: " ^ t.name)
        | Some x -> x)

  let linker ?linkers t =
    match t.command with
    | Some cmd ->
        Linker.
          {
            name = t.name;
            link_type = Linker.link_type_of_string t.link_type;
            exts = String_set.of_list t.ext;
            has_runtime = t.has_runtime;
            wrap_c_flags = wrap_c_flags t;
            command =
              (fun ~flags ~objs ~output ->
                List.concat_map
                  (fun x ->
                    if String.equal x "#objs" then
                      let objs =
                        List.map
                          (fun obj -> Eio.Path.native_exn obj.Object_file.path)
                          objs
                      in
                      objs
                    else if String.equal x "#output" then
                      [ Eio.Path.native_exn output ]
                    else if String.equal x "#flags" then flags.link
                    else [ x ])
                  cmd);
          }
    | None -> (
        match
          Linker.find_by_name (Option.value ~default:!Linker.all linkers) t.name
        with
        | None -> invalid_arg ("unknown linker: " ^ t.name)
        | Some x -> x)
end

module Build_config = struct
  module Lang_flags = struct
    type t = {
      lang : string;
      compile : string list; [@default []]
      link : string list; [@default []]
      all : string list; [@default []]
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
  List.map (fun c -> Compiler_config.named c.Compiler.name) Compiler.default

let default_linkers =
  List.map (fun c -> Compiler_config.named c.Linker.name) Linker.default

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

(* Search for config file in current directory and parent directories *)
let rec find_config_in_parents path =
  let yml_path = Eio.Path.(path / "zenon.yml") in
  let yaml_path = Eio.Path.(path / "zenon.yaml") in

  if Eio.Path.is_file yml_path then Some yml_path
  else if Eio.Path.is_file yaml_path then Some yaml_path
  else
    let native = Eio.Path.native_exn path in
    (* Stop if we're at the root *)
    if
      String.equal native "/" || String.equal native "."
      || String.equal native ""
    then None
    else
      match Eio.Path.split path with
      | None -> None
      | Some (parent_path, _) ->
          let parent_native = Eio.Path.native_exn parent_path in
          if String.equal native parent_native then None
          else find_config_in_parents parent_path

let read_file_or_default path =
  if Eio.Path.is_file path then read_file path
  else if Eio.Path.is_directory path then
    match find_config_in_parents path with
    | Some config_path -> read_file config_path
    | None -> Ok (empty, Unix.gettimeofday ())
  else Ok (empty, Unix.gettimeofday ())

let init ?mtime ~env ~log_level path t =
  let () =
    List.iter
      (fun c -> Compiler.register @@ Compiler_config.compiler ~compilers:[] c)
      t.tools.compilers
  in
  let () =
    List.iter
      (fun l -> Linker.register @@ Compiler_config.linker ~linkers:[] l)
      t.tools.linkers
  in

  let gitignore_patterns =
    Util.parse_gitignore Eio.Path.(path / ".gitignore")
  in

  let config_ignore_patterns = List.map Util.glob t.ignore in

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
        let compilers =
          if List.is_empty config.compilers then !Compiler.all
          else
            List.filter_map
              (fun name -> Compiler.find_by_name name)
              config.compilers
        in
        let linker_name =
          match config.Build_config.linker with
          | Some linker -> Some linker (* User-specified linker *)
          | None -> (
              match config.target with
              | Some target when Util.is_static_lib (Filename.basename target)
                ->
                  Some "ar"
              | Some target when Util.is_shared_lib (Filename.basename target)
                ->
                  let has_cxx =
                    List.exists
                      (fun name -> name = "clang++" || name = "g++")
                      config.compilers
                  in
                  let linker_name =
                    if has_cxx then Some "clang++-shared"
                    else Some "clang-shared"
                  in
                  linker_name
              | _ -> None)
        in
        let linker =
          Option.map
            (fun linker_name ->
              Compiler_config.linker @@ Compiler_config.named linker_name)
            linker_name
        in
        let compiler_flags =
          let tbl = Hashtbl.create 8 in
          let add_flags flags =
            List.iter
              (fun (f : Build_config.Lang_flags.t) ->
                let lang = f.lang in
                let compile = f.compile @ f.all in
                let link = f.link @ f.all in
                let existing =
                  Option.value (Hashtbl.find_opt tbl lang) ~default:(Flags.v ())
                in
                let new_flags =
                  Flags.v
                    ~compile:(existing.compile @ compile)
                    ~link:(existing.link @ link) ()
                in
                Hashtbl.replace tbl lang new_flags)
              flags
          in
          add_flags t.flags;
          add_flags config.flags;
          List.of_seq (Hashtbl.to_seq tbl)
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
              match config.target with
              | Some p -> Filename.basename p
              | None -> "default")
        in
        let build_ignore_patterns = List.map Util.glob config.ignore in
        let all_ignore =
          gitignore_patterns @ config_ignore_patterns @ build_ignore_patterns
        in
        let build =
          Build.v ~parallel:config.parallel ?script:config.script
            ~pkgconf:(t.pkgconf @ config.pkgconf)
            ?after:config.after ~depends_on:config.depends_on ?linker ~compilers
            ~compiler_flags ?output ~source ~files:(t.files @ config.files)
            ~headers:config.headers ~name ~ignore:all_ignore
            ~hidden:config.hidden ~log_level ?mtime env
        in
        Some build)
    t.build

let load ~env ~log_level path =
  let project_root =
    if Eio.Path.is_directory path then path
    else
      match Eio.Path.split path with None -> assert false | Some (p, _) -> p
  in
  match read_file_or_default path with
  | Ok (config, mtime) -> Ok (init ~mtime ~env ~log_level project_root config)
  | Error e -> Error e
