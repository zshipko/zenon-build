open Types
open Source_file
open Object_file

type link_type = Executable | Shared | Static

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

type t = {
  name : string;
  link_type : link_type;
  exts : String_set.t;
  has_runtime : bool;
  command :
    flags:Flags.t -> objs:Object_file.t list -> output:path -> string list;
}

let link t mgr ~output ~objs ~flags ~build_dir =
  Util.mkparent output;
  let cmd = t.command ~flags ~objs ~output in
  Log_file.with_log_file ~build_dir
    ~name:(Digest.to_hex (Digest.string (String.concat " " cmd)))
  @@ fun (tmp_path, log_file) ->
  try Eio.Process.run mgr cmd ~stdout:log_file ~stderr:log_file
  with exn ->
    (try
       let log = Eio.Path.load tmp_path in
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
  }

let clang_shared =
  {
    name = "clang";
    exts = String_set.empty;
    has_runtime = false;
    command = c_like [ "clang"; "-shared" ];
    link_type = Shared;
  }

let clangxx =
  {
    name = "clang++";
    exts = Compiler.clangxx.ext;
    has_runtime = true;
    command = c_like [ "clang++" ];
    link_type = Executable;
  }

let clangxx_shared =
  {
    name = "clang++";
    exts = Compiler.clangxx.ext;
    has_runtime = true;
    command = c_like [ "clang++"; "-shared" ];
    link_type = Shared;
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
        [ "ghc"; "-v0"; "-o"; Eio.Path.native_exn output ]
        @ include_paths @ flags.Flags.link @ objs);
    link_type = Executable;
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
        [ "mlton"; "-output"; Eio.Path.native_exn output ]
        @ List.concat_map (fun x -> [ "-link-opt"; x ]) flags.Flags.link
        @ objs);
    link_type = Executable;
  }

let ats2 =
  {
    name = "patscc";
    exts = Compiler.ats2.ext;
    has_runtime = true;
    command = c_like [ "patscc" ];
    link_type = Executable;
  }

let flang =
  {
    name = "flang-new";
    exts = Compiler.flang.ext;
    has_runtime = false;
    command = c_like [ "flang-new" ];
    link_type = Executable;
  }

let default =
  [ clang; clang_shared; clangxx; clangxx_shared; ar; ghc; mlton; ats2; flang ]

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
      | _ -> None)

let match_linker ~source_exts linkers =
  let matching_linkers =
    List.filter
      (fun linker ->
        linker.has_runtime
        && not (String_set.is_empty (String_set.inter linker.exts source_exts)))
      linkers
  in
  match matching_linkers with
  | [] -> Ok None
  | [ linker ] -> Ok (Some linker)
  | linkers ->
      Error
        (Printf.sprintf
           "conflicting linkers detected. You will need to explicitly specify \
            a linker. linkers detected: %s"
           (String.concat ", " (List.map (fun l -> l.name) linkers)))

let auto_select_linker ~sources ?(linker = clang) () =
  if linker.has_runtime then linker
  else
    let source_exts =
      List.fold_left
        (fun acc src -> String_set.add (Source_file.ext src) acc)
        String_set.empty sources
    in
    match match_linker ~source_exts default with
    | Ok (Some linker) -> linker
    | _ -> (
        match match_linker ~source_exts !all with
        | Ok (Some linker) -> linker
        | Ok None -> linker
        | Error msg -> Fmt.failwith "%s" msg)
