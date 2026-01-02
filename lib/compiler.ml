open Types

type t = {
  name : string;
  command :
    flags:Flags.t ->
    sources:Source_file.t list ->
    output:Object_file.t ->
    string list;
  ext : String_set.t;
}

let c_like cc =
 fun ~flags ~sources:_ ~output ->
  cc
  @ [ "-c"; "-o"; Eio.Path.native_exn output.Object_file.path ]
  @ flags.Flags.compile
  @ [ Eio.Path.native_exn output.source.path ]

let clang =
  {
    name = "clang";
    command = c_like [ "clang" ];
    ext = String_set.of_list [ "c"; "s"; "ll"; "bc" ];
  }

let clangxx =
  {
    name = "clang++";
    command = c_like [ "clang++" ];
    ext = String_set.of_list [ "cc"; "cpp" ];
  }

let ispc =
  {
    name = "ispc";
    command =
      (fun ~flags ~sources:_ ~output ->
        [
          "ispc";
          "--emit-obj";
          "-o";
          Eio.Path.native_exn output.Object_file.path;
        ]
        @ flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "ispc" ];
  }

let ghc =
  {
    name = "ghc";
    command =
      (fun ~flags ~sources ~output ->
        let include_paths =
          List.filter_map
            (fun source ->
              match Eio.Path.split source.Source_file.path with
              | Some (parent, _) -> Some ("-i" ^ Eio.Path.native_exn @@ parent)
              | None -> None)
            sources
        in
        let hidir =
          match Eio.Path.split output.Object_file.path with
          | None -> []
          | Some (parent, _) -> [ "-hidir"; Eio.Path.native_exn parent ]
        in
        [
          "ghc"; "-v0"; "-c"; "-o"; Eio.Path.native_exn output.Object_file.path;
        ]
        @ hidir @ include_paths @ flags.Flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "hs"; "lhs" ];
  }

let compile_obj t mgr ~sources ~sw ~output ~build_mtime flags =
  let st =
    try
      Option.some
        ( Eio.Path.stat ~follow:true output.Object_file.path,
          Eio.Path.stat ~follow:true output.source.path )
    with _ -> None
  in
  match st with
  | Some (obj, src) when obj.mtime > src.mtime && obj.mtime > build_mtime ->
      Util.log "• CACHE %s -> %s"
        (Eio.Path.native_exn output.source.path)
        (Eio.Path.native_exn output.Object_file.path);
      None
  | _ ->
      Util.log "• BUILD(%s) %s -> %s" t.name
        (Eio.Path.native_exn output.source.path)
        (Eio.Path.native_exn output.Object_file.path);
      Util.mkparent output.Object_file.path;
      let cmd = t.command ~sources ~flags ~output in
      Some (Eio.Process.spawn mgr cmd ~sw)

let find_by_name compilers c =
  match List.find_opt (fun x -> x.name = c) compilers with
  | Some x -> Some x
  | None -> (
      match c with
      | "c" | "cc" | "clang" -> Some clang
      | "clang++" | "c++" | "cxx" | "cpp" -> Some clangxx
      | "ispc" -> Some ispc
      | "ghc" | "hs" | "lhs" -> Some ghc
      | _ -> None)
