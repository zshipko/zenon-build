open Types
open Source_file
open Object_file

type link_type =
  | Executable [@name "exe"]
  | Shared [@name "shared"]
  | Static [@name "static"]
[@@deriving yaml]

type t = {
  name : string;
  link_type : link_type;
  command :
    flags:Flags.t -> objs:Object_file.t list -> output:path -> string list;
}

let link t mgr ~output ~objs ~flags =
  Util.mkparent output;
  let cmd = t.command ~flags ~objs ~output in
  Eio.Process.run mgr cmd

let c_like cc =
 fun ~flags ~objs ~output ->
  let objs =
    List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
  in
  cc @ [ "-o"; Eio.Path.native_exn output ] @ flags.Flags.link @ objs

let clang =
  { name = "clang"; command = c_like [ "clang" ]; link_type = Executable }

let clang_shared =
  {
    name = "clang";
    command = c_like [ "clang"; "-shared" ];
    link_type = Shared;
  }

let clangxx =
  { name = "clang++"; command = c_like [ "clang++" ]; link_type = Executable }

let clangxx_shared =
  {
    name = "clang++";
    command = c_like [ "clang++"; "-shared" ];
    link_type = Shared;
  }

let ape =
  { name = "apelink"; command = c_like [ "apelink" ]; link_type = Executable }

let ar =
  {
    name = "ar";
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
    command =
      (fun ~flags ~objs ~output ->
        let objs =
          List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
        in
        [ "mlton"; "-o"; Eio.Path.native_exn output ]
        @ (List.map (fun x -> [ "-link-opt"; x ]) flags.Flags.link
          |> List.flatten)
        @ objs);
    link_type = Executable;
  }

let ats2 =
  { name = "patscc"; command = c_like [ "patscc" ]; link_type = Executable }

let flang =
  {
    name = "flang-new";
    command = c_like [ "flang-new" ];
    link_type = Executable;
  }

let gcc = { name = "gcc"; command = c_like [ "gcc" ]; link_type = Executable }
let gxx = { name = "g++"; command = c_like [ "g++" ]; link_type = Executable }

let gfortran =
  { name = "gfortran"; command = c_like [ "gfortran" ]; link_type = Executable }

let find_by_name linkers l =
  match List.find_opt (fun x -> x.name = l) linkers with
  | Some x -> Some x
  | None -> (
      match l with
      | "c" | "cc" | "clang" -> Some clang
      | "shared" | "so" | "dylib" -> Some clang_shared
      | "clang++" | "c++" | "cxx" | "cpp" -> Some clangxx
      | "clang++-shared" -> Some clangxx_shared
      | "ar" | "static" | "staticlib" -> Some ar
      | "ghc" | "hs" | "lhs" -> Some ghc
      | "flang-new" | "flang" | "fortran" -> Some flang
      | "gcc" -> Some gcc
      | "g++" | "gxx" -> Some gxx
      | "gfortran" -> Some gfortran
      | "ape" | "apelink" -> Some ape
      | "sml" | "mlton" -> Some mlton
      | "ats2" | "ats" | "pats" | "patscc" -> Some ats2
      | _ -> None)
