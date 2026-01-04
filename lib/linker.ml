open Types
open Source_file
open Object_file

type link_type = Executable | Shared | Static

let link_type_of_string = function
  | "exe" | "executable" | "exec" | "bin" -> Executable
  | "shared" | "so" | "dylib" -> Shared
  | "static" | "archive" -> Static
  | other -> invalid_arg ("invalid link type: " ^ other)

type t = {
  name : string;
  link_type : link_type;
  exts : String_set.t;
  has_runtime : bool;
  command :
    flags:Flags.t -> objs:Object_file.t list -> output:path -> string list;
}

let link t mgr ~sw ~fs ~output ~objs ~flags =
  Util.mkparent output;
  let cmd = t.command ~flags ~objs ~output in
  (* Create log file for linker output *)
  let log_path =
    let out_path = Eio.Path.native_exn output in
    Eio.Path.(fs / (out_path ^ ".log"))
  in
  Util.mkparent log_path;
  let log_file = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) log_path in
  Eio.Process.run mgr cmd ~stdout:log_file ~stderr:log_file;
  log_path

let c_like cc =
 fun ~flags ~objs ~output ->
  let objs =
    List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
  in
  cc @ [ "-o"; Eio.Path.native_exn output ] @ flags.Flags.link @ objs

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

let cosmocc =
  {
    name = "cosmocc";
    exts = String_set.empty;
    has_runtime = false;
    command = c_like [ "cosmocc" ];
    link_type = Executable;
  }

let cosmocxx =
  {
    name = "cosmoc++";
    exts = String_set.empty;
    has_runtime = false;
    command = c_like [ "cosmoc++" ];
    link_type = Executable;
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

let gcc =
  {
    name = "gcc";
    exts = Compiler.gcc.ext;
    has_runtime = false;
    command = c_like [ "gcc" ];
    link_type = Executable;
  }

let gxx =
  {
    name = "g++";
    exts = Compiler.gxx.ext;
    has_runtime = true;
    command = c_like [ "g++" ];
    link_type = Executable;
  }

let gcc_shared =
  {
    name = "gcc";
    exts = Compiler.gcc.ext;
    has_runtime = false;
    command = c_like [ "gcc"; "-shared" ];
    link_type = Shared;
  }

let gxx_shared =
  {
    name = "g++";
    exts = Compiler.gxx.ext;
    has_runtime = true;
    command = c_like [ "g++"; "-shared" ];
    link_type = Shared;
  }

let gfortran =
  {
    name = "gfortran";
    exts = Compiler.gfortran.ext;
    has_runtime = false;
    command = c_like [ "gfortran" ];
    link_type = Executable;
  }

let gfortran_shared =
  {
    name = "gfortran";
    exts = Compiler.gfortran.ext;
    has_runtime = false;
    command = c_like [ "gfortran"; "-shared" ];
    link_type = Executable;
  }

let builtin =
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
    cosmocc;
    cosmocxx;
    gcc;
    gxx;
    gfortran;
  ]

let all = ref builtin

let default =
  [ clang; clang_shared; clangxx; clangxx_shared; ar; ghc; mlton; ats2; flang ]

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
      | "gcc" -> Some gcc
      | "g++" -> Some gxx
      | "g++-shared" -> Some gxx_shared
      | "gfortran" -> Some gfortran
      | "cosmocc" -> Some cosmocc
      | "cosmoc++" -> Some cosmocxx
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
  | [] -> Ok None (* No runtime-specific linker needed *)
  | [ linker ] -> Ok (Some linker) (* Exactly one match *)
  | linkers ->
      Error
        (Printf.sprintf
           "conflicting linkers detected. You will need to explicitly specify \
            a linker. linkers detected: %s"
           (String.concat ", " (List.map (fun l -> l.name) linkers)))

let auto_select_linker ~sources ?(linker = clang) () =
  match linker.name with
  | "clang" | "gcc" -> (
      (* Only auto-select if using generic C linker *)
      let source_exts =
        List.fold_left
          (fun acc src -> String_set.add (Source_file.ext src) acc)
          String_set.empty sources
      in
      (* Use all available linkers for auto-selection, including custom ones *)
      match match_linker ~source_exts builtin with
      | Ok (Some linker) -> linker
      | _ -> (
          match match_linker ~source_exts !all with
          | Ok (Some linker) -> linker
          | Ok None -> linker (* No specialized linker needed *)
          | Error msg -> Fmt.failwith "%s" msg))
  | _ -> linker
