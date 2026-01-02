open Types

module Flags = struct
  type t = { mutable compile : string list; mutable link : string list }

  let v ?(compile = []) ?(link = []) () = { compile; link }
  let add_compile_flags t flags = t.compile <- t.compile @ flags
  let add_link_flags t flags = t.link <- t.link @ flags
  let concat a b = { compile = a.compile @ b.compile; link = a.link @ b.link }
end

module Source_file = struct
  type t = { path : path; flags : Flags.t; root : path }

  let v ?flags ~root path =
    { path; flags = Option.value ~default:(Flags.v ()) flags; root }

  let ext { path; _ } = Util.ext path
end

module Object_file = struct
  type t = { source : Source_file.t; path : path; flags : Flags.t }

  let v ?flags ~source path =
    { source; path; flags = Option.value ~default:(Flags.v ()) flags }

  let of_source ?flags ~build_dir source =
    let obj_file =
      Util.relative_to source.Source_file.root source.Source_file.path ^ ".o"
    in
    v ?flags ~source @@ Eio.Path.(build_dir / obj_file)
end

module Linker = struct
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
                match Eio.Path.split x.Object_file.source.path with
                | Some (parent, _) -> Some ("-i" ^ Eio.Path.native_exn @@ parent)
                | None -> None)
              objs
          in
          let objs =
            List.map (fun obj -> Eio.Path.native_exn obj.Object_file.path) objs
          in
          [ "ghc"; "-o"; Eio.Path.native_exn output ]
          @ include_paths @ flags.Flags.link @ objs);
      link_type = Executable;
    }
end

module Compiler = struct
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
          [ "ghc"; "-c"; "-o"; Eio.Path.native_exn output.Object_file.path ]
          @ include_paths @ flags.Flags.compile
          @ [ Eio.Path.native_exn output.source.path ]);
      ext = String_set.of_list [ "hs"; "lhs" ];
    }

  let compile_obj t mgr ~sources ~sw ~output ~build_mtime flags =
    let st =
      try
        Option.some
        @@ ( Eio.Path.stat ~follow:true output.Object_file.path,
             Eio.Path.stat ~follow:true output.source.path )
      with _ -> None
    in
    match st with
    | Some (obj, src) when obj.mtime > src.mtime && obj.mtime > build_mtime ->
        Util.log "• CACHE %s -> %s"
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        None
    | _ ->
        Util.log "• BUILD(%s) %s -> %s" t.name
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        Util.mkparent output.Object_file.path;
        let cmd = t.command ~sources ~flags ~output in
        Some (Eio.Process.spawn mgr cmd ~sw)
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String_set.compare a.Compiler.ext b.Compiler.ext
  end)

  let default = of_list Compiler.[ clang; clangxx; ispc ]

  let default_ext =
    fold (fun x -> String_set.union x.ext) default String_set.empty
end
