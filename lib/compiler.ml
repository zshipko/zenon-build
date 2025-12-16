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
    let obj_file = Util.with_ext source.Source_file.path "o" in
    let p =
      Eio.Path.split source.root |> Option.map fst
      |> Option.value ~default:source.root
    in
    let obj_file = Eio.Path.(build_dir / Util.relative_to p obj_file) in
    v ?flags ~source @@ obj_file
end

module Compiler = struct
  type t = {
    command : string list;
    ext : String_set.t;
    out_flag : string;
    obj_flag : string;
    obj_ext : string option;
  }

  let clang =
    {
      command = [ "clang" ];
      out_flag = "-o";
      obj_flag = "-c";
      ext = String_set.of_list [ "c" ];
      obj_ext = None;
    }

  let clangxx =
    {
      clang with
      command = [ "clang++" ];
      ext = String_set.of_list [ "cc"; "cpp" ];
    }

  let zig_cc = { clang with command = [ "zig"; "cc" ] }

  let ispc =
    {
      clang with
      command = [ "ispc" ];
      obj_flag = "--emit-obj";
      ext = String_set.of_list [ "ispc" ];
    }

  let ghc =
    { clang with command = [ "ghc" ]; ext = String_set.of_list [ "hs"; "lhs" ] }

  let ocaml =
    {
      command = [ "ocamlfind"; "ocamlopt" ];
      obj_flag = "-c";
      out_flag = "-o";
      obj_ext = Some "cmx";
      ext = String_set.of_list [ "ml" ];
    }

  let compile_obj t mgr ~sw ?(objs = []) ~output args =
    let st =
      try
        Option.some
        @@ ( Eio.Path.stat ~follow:true output.Object_file.path,
             Eio.Path.stat ~follow:true output.source.path )
      with _ -> None
    in
    match st with
    | Some (obj, src) when obj.mtime > src.mtime ->
        Util.log "• CACHE %s (%s)"
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        None
    | _ ->
        Util.log "• BUILD %s -> %s"
          (Eio.Path.native_exn output.source.path)
          (Eio.Path.native_exn output.path);
        Util.mkparent output.Object_file.path;
        let cmd =
          t.command @ args @ objs
          @ [
              t.obj_flag;
              Eio.Path.native_exn output.Object_file.source.path;
              t.out_flag;
              Eio.Path.native_exn output.Object_file.path;
            ]
        in
        Some (Eio.Process.spawn mgr cmd ~sw)

  let link t mgr objs ~output ~compiler_index args =
    Util.mkparent output;
    let objs =
      List.map
        (fun f ->
          let c =
            Hashtbl.find compiler_index @@ Source_file.ext f.Object_file.source
          in
          let path =
            match c.obj_ext with
            | Some ext -> Util.with_ext f.path ext
            | None -> f.path
          in
          Eio.Path.native_exn path)
        objs
    in
    let args =
      t.command @ args @ objs @ [ t.out_flag; Eio.Path.native_exn output ]
    in
    Eio.Process.run mgr args
end

module Compiler_set = struct
  include Set.Make (struct
    type t = Compiler.t

    let compare a b = String_set.compare a.Compiler.ext b.Compiler.ext
  end)

  let default = of_list Compiler.[ clang; ispc; ocaml; ghc ]

  let default_ext =
    fold (fun x -> String_set.union x.ext) default String_set.empty
end
