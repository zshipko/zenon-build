open Types
open Source_file
open Object_file

type link_type = Executable | Shared | Static [@@deriving yaml]

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

let find_by_name linkers l =
  match List.find_opt (fun x -> x.name = l) linkers with
  | Some x -> Some x
  | None -> (
      match l with
      | "c" | "cc" | "clang" -> Some clang
      | "shared" | "so" | "dylib" -> Some clang_shared
      | "clang++" | "c++" | "cxx" | "cpp" -> Some clangxx
      | "ar" | "static" | "staticlib" -> Some ar
      | "ghc" | "hs" | "lhs" -> Some ghc
      | _ -> None)
