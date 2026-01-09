open Types
open Source_file
open Object_file

type link_type = Executable | Shared | Static

type t = {
  name : string;
  link_type : link_type;
  exts : String_set.t;
  has_runtime : bool;
  command :
    flags:Flags.t -> objs:Object_file.t list -> output:path -> string list;
  wrap_c_flags : Flags.t -> Flags.t;
}

let link_type_of_string s =
  match String.lowercase_ascii s with
  | "exe" | "executable" | "exec" | "bin" -> Executable
  | "shared" | "so" | "dylib" -> Shared
  | "static" | "archive" -> Static
  | other -> invalid_arg ("invalid link type: " ^ other)

let string_of_link_type = function
  | Executable -> "exe"
  | Shared -> "shared"
  | Static -> "static"

let link t mgr ~output ~objs ~flags ~build_dir =
  Util.mkparent output;
  let cmd = t.command ~flags ~objs ~output in
  Log_file.with_log_file ~build_dir
    ~name:(Digest.to_hex (Digest.string (String.concat " " cmd)))
  @@ fun (tmp_path, log_file) ->
  try Eio.Process.run mgr cmd ~stdout:log_file ~stderr:log_file
  with exn ->
    (try
       let log = Log_file.get tmp_path in
       Util.log_clear "❌ linker failed: %s\n%s" (String.concat " " cmd) log
     with _ -> ());
    raise exn

let c_like ?(force_color = "-fdiagnostics-color") cc =
 fun ~flags ~objs ~output ->
  let objs =
    List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
  in
  cc
  @ [ force_color; "-o"; Eio.Path.native_exn output ]
  @ flags.Flags.link @ objs

let clang =
  {
    name = "clang";
    exts = String_set.empty;
    has_runtime = false;
    command = c_like [ "clang" ];
    link_type = Executable;
    wrap_c_flags = Compiler.clang.wrap_c_flags;
  }

let clang_shared =
  {
    name = "clang";
    exts = String_set.empty;
    has_runtime = false;
    command = c_like [ "clang"; "-shared" ];
    link_type = Shared;
    wrap_c_flags = Compiler.clang.wrap_c_flags;
  }

let clangxx =
  {
    name = "clang++";
    exts = Compiler.clangxx.ext;
    has_runtime = true;
    command = c_like [ "clang++" ];
    link_type = Executable;
    wrap_c_flags = Compiler.clangxx.wrap_c_flags;
  }

let clangxx_shared =
  {
    name = "clang++";
    exts = Compiler.clangxx.ext;
    has_runtime = true;
    command = c_like [ "clang++"; "-shared" ];
    link_type = Shared;
    wrap_c_flags = Compiler.clangxx.wrap_c_flags;
  }

let ar =
  {
    name = "ar";
    exts = String_set.empty;
    has_runtime = false;
    link_type = Static;
    command =
      (fun ~flags:_ ~objs ~output ->
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [ "ar"; "rcs"; Eio.Path.native_exn output ] @ objs);
    wrap_c_flags = Fun.id;
  }

let ghc =
  {
    name = "ghc";
    exts = Compiler.ghc.ext;
    has_runtime = true;
    command =
      (fun ~flags ~objs ~output ->
        let include_paths =
          List.filter_map
            (fun x ->
              match Eio.Path.split x.source.path with
              | Some (parent, _) -> Some ("-i" ^ Eio.Path.native_exn @@ parent)
              | None -> None)
            objs
        in
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [
          "ghc";
          "-pgmc";
          "clang";
          "-pgml";
          "clang";
          "-v0";
          "-fdiagnostics-color=always";
          "-package";
          "base";
          "-package";
          "text";
          "-o";
          Eio.Path.native_exn output;
        ]
        @ include_paths @ flags.Flags.link @ objs);
    link_type = Executable;
    wrap_c_flags = Compiler.ghc.wrap_c_flags;
  }

let ocaml =
  {
    name = "ocamlfind";
    exts = Compiler.ocaml.ext;
    has_runtime = true;
    command =
      (fun ~flags ~objs ~output ->
        let include_paths =
          List.concat_map
            (fun x ->
              match Eio.Path.split x.path with
              | Some (parent, _) -> [ "-I"; Eio.Path.native_exn @@ parent ]
              | None -> [])
            objs
        in
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [
          "ocamlfind";
          "ocamlopt";
          "-cc";
          "clang";
          "-I";
          "+unix";
          "unix.cmxa";
          "-color=always";
          "-linkall";
          "-o";
          Eio.Path.native_exn output;
        ]
        @ include_paths @ flags.Flags.link @ objs);
    link_type = Executable;
    wrap_c_flags = Compiler.ocaml.wrap_c_flags;
  }

let ocaml_lib =
  {
    name = "ocamlfind";
    exts = Compiler.ocaml.ext;
    has_runtime = true;
    command =
      (fun ~flags ~objs ~output ->
        let include_paths =
          List.concat_map
            (fun x ->
              match Eio.Path.split x.path with
              | Some (parent, _) -> [ "-I"; Eio.Path.native_exn @@ parent ]
              | None -> [])
            objs
        in
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [
          "ocamlfind";
          "ocamlopt";
          "-a";
          "-cc";
          "clang";
          "-I";
          "+unix";
          "-color=always";
          "-linkall";
          "-o";
          Eio.Path.native_exn output;
        ]
        @ include_paths @ flags.Flags.link @ objs);
    link_type = Static;
    wrap_c_flags = Compiler.ocaml.wrap_c_flags;
  }

let mlton =
  {
    name = "mlton";
    exts = Compiler.mlton.ext;
    has_runtime = true;
    command =
      (fun ~flags ~objs ~output ->
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [ "mlton"; "-cc"; "clang"; "-output"; Eio.Path.native_exn output ]
        @ flags.Flags.link (* Wrapping now done by wrap_c_flags *) @ objs);
    link_type = Executable;
    wrap_c_flags = Compiler.mlton.wrap_c_flags;
  }

let ats2 =
  {
    name = "patscc";
    exts = Compiler.ats2.ext;
    has_runtime = true;
    command = c_like [ "patscc" ];
    link_type = Executable;
    wrap_c_flags = Compiler.ats2.wrap_c_flags;
  }

let flang =
  {
    name = "flang-new";
    exts = Compiler.flang.ext;
    has_runtime = false;
    command = c_like [ "flang-new" ];
    link_type = Executable;
    wrap_c_flags = Compiler.flang.wrap_c_flags;
  }

let default =
  [
    clang;
    clang_shared;
    clangxx;
    clangxx_shared;
    ar;
    ghc;
    mlton;
    ats2;
    flang;
    ocaml;
    ocaml_lib;
  ]

let all = ref default

let register linker =
  if
    not
      (List.exists
         (fun l -> l.name = linker.name && l.link_type = linker.link_type)
         !all)
  then all := linker :: !all

let find_by_name linkers l =
  match List.find_opt (fun x -> x.name = l) linkers with
  | Some x -> Some x
  | None -> (
      match l with
      | "c" | "cc" | "clang" -> Some clang
      | "clang-shared" | "shared" | "so" | "dylib" -> Some clang_shared
      | "clang++" | "c++" | "cxx" | "cpp" -> Some clangxx
      | "clang++-shared" -> Some clangxx_shared
      | "ar" | "static" | "staticlib" -> Some ar
      | "ghc" | "hs" | "lhs" -> Some ghc
      | "flang-new" | "flang" | "fortran" -> Some flang
      | "sml" | "mlton" -> Some mlton
      | "ats2" | "ats" | "pats" | "patscc" -> Some ats2
      | "ocaml" | "ml" | "ocamlfind" | "ocamlopt" -> Some ocaml
      | "ocaml-lib" | "ocaml-static" -> Some ocaml_lib
      | _ -> None)

let match_linker ~target ~source_exts linkers =
  let matching_linkers =
    List.filter
      (fun linker ->
        linker.has_runtime
        && not (String_set.is_empty (String_set.inter linker.exts source_exts)))
      linkers
    |> List.sort_uniq (fun a b -> String.compare a.name b.name)
  in
  match matching_linkers with
  | [] -> Ok None
  | [ linker ] -> Ok (Some linker)
  | linkers ->
      Error
        (Printf.sprintf
           "conflicting linkers detected in target '%s'. You will need to \
            explicitly specify a linker. linkers detected: %s"
           target
           (String.concat ", " (List.map (fun l -> l.name) linkers)))

let auto_select_linker ~sources ?output ?(linker = clang) target =
  if linker.has_runtime then linker
  else if
    Option.map
      (fun output ->
        String.ends_with ~suffix:".cmxa" (Eio.Path.native_exn output))
      output
    |> Option.value ~default:false
  then ocaml_lib
  else
    let source_exts =
      List.fold_left
        (fun acc src -> String_set.add (Source_file.ext src) acc)
        String_set.empty sources
    in
    match match_linker ~target ~source_exts default with
    | Ok (Some linker) -> linker
    | _ -> (
        match match_linker ~target ~source_exts !all with
        | Ok (Some linker) -> linker
        | Ok None -> linker
        | Error msg -> Fmt.failwith "%s" msg)
