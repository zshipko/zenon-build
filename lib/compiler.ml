open Types

type t = {
  name : string;
  command :
    flags:Flags.t ->
    objects:Object_file.t Seq.t ->
    output:Object_file.t ->
    string list;
  transform_output : Object_file.t -> Object_file.t;
  parallel : bool;
  ext : String_set.t;
  wrap_c_flags : Flags.t -> Flags.t;
}

let c_like ?(force_color = "-fcolor-diagnostics") cc =
 fun ~flags ~objects:_ ~output ->
  cc
  @ [ force_color; "-c"; "-o"; Eio.Path.native_exn output.Object_file.path ]
  @ flags.Flags.compile
  @ [ Eio.Path.native_exn output.source.path ]

let clang =
  {
    name = "clang";
    command = c_like [ "clang" ];
    ext = String_set.of_list [ "c"; "s"; "ll"; "bc" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags = Fun.id;
  }

let clangxx =
  {
    name = "clang++";
    command = c_like [ "clang++" ];
    ext = String_set.of_list [ "cc"; "cpp"; "cxx" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags = Fun.id;
  }

let ispc =
  {
    name = "ispc";
    command =
      (fun ~flags ~objects:_ ~output ->
        [
          "ispc";
          "--colored-output";
          "--emit-obj";
          "-o";
          Eio.Path.native_exn output.Object_file.path;
        ]
        @ flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "ispc" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags = Fun.id;
  }

let ghc =
  {
    name = "ghc";
    command =
      (fun ~flags ~objects ~output ->
        let hidir =
          match Eio.Path.split output.Object_file.path with
          | None -> []
          | Some (p, _) -> [ "-hidir"; Eio.Path.native_exn p ]
        in
        let include_paths =
          Seq.filter_map
            (fun obj ->
              match Eio.Path.split obj.Object_file.path with
              | None -> None
              | Some (p, _) -> Some ("-i" ^ Eio.Path.native_exn p))
            objects
          |> List.of_seq
        in
        [
          "ghc";
          "-pgmc";
          "clang";
          "-fdiagnostics-color=always";
          "-v0";
          "-package";
          "base";
          "-package";
          "text";
          "-c";
          "-o";
          Eio.Path.native_exn output.Object_file.path;
        ]
        @ hidir @ include_paths @ flags.Flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "hs"; "lhs" ];
    transform_output = Fun.id;
    parallel = false;
    wrap_c_flags =
      (fun flags ->
        let compile =
          List.concat_map (fun x -> [ "-optc" ^ x ]) flags.Flags.compile
        in
        let link = List.concat_map (fun x -> [ "-optl" ^ x ]) flags.link in
        Flags.v ~compile ~link ());
  }

let ocaml =
  {
    name = "ocamlfind";
    command =
      (fun ~flags ~objects ~output ->
        let p, s = output.Object_file.path in
        let o_path =
          let s = Filename.chop_extension s ^ ".o" in
          (p, s)
        in
        let include_paths =
          List.concat_map
            (fun obj ->
              match Eio.Path.split obj.Object_file.path with
              | None -> []
              | Some (p, _) -> [ "-I"; Eio.Path.native_exn p ])
            (List.of_seq objects)
        in
        [
          "ocamlfind";
          "ocamlopt";
          "-cc";
          "clang";
          "-I";
          "+unix";
          "-color=always";
          "-c";
          "-o";
          Eio.Path.native_exn o_path;
        ]
        @ include_paths @ flags.Flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "ml" ];
    transform_output =
      (fun obj ->
        let p, s = obj.path in
        let dest =
          (Filename.chop_extension @@ Filename.chop_extension @@ s) ^ ".cmx"
        in
        { obj with path = (p, dest) });
    parallel = false;
    wrap_c_flags =
      (fun flags ->
        let compile =
          List.concat_map (fun x -> [ "-cclib"; x ]) flags.Flags.compile
        in
        let link = List.concat_map (fun x -> [ "-ccopt"; x ]) flags.link in
        Flags.v ~compile ~link ());
  }

let mlton =
  {
    name = "mlton";
    command =
      (fun ~flags ~objects:_ ~output ->
        let out =
          Filename.quote (Eio.Path.native_exn output.Object_file.path)
        in
        let src = Filename.quote (Eio.Path.native_exn output.source.path) in
        let args =
          [ "mlton"; "-cc"; "clang"; "-stop"; "o"; "-output"; out ]
          @ flags.compile @ [ src ]
        in
        [
          "sh";
          "-c";
          String.concat " " args
          ^ Printf.sprintf
              " && ld -r %s.0.o %s.1.o -o %s && rm \
               %s.0.o %s.1.o"
              out out out out out;
        ]);
    ext = String_set.of_list [ "sml"; "mlb" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags =
      (fun flags ->
        let compile =
          List.concat_map (fun x -> [ "-cc-opt"; x ]) flags.Flags.compile
        in
        let link = List.concat_map (fun x -> [ "-link-opt"; x ]) flags.link in
        Flags.v ~compile ~link ());
  }

let ats2 =
  {
    name = "patscc";
    command =
      c_like [ "patscc"; "-Wno-unused-command-line-argument"; "-cleanaft" ];
    ext = String_set.of_list [ "dats"; "sats" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags = Fun.id;
  }

let flang =
  {
    name = "flang-new";
    command = c_like [ "flang-new" ];
    ext = String_set.of_list [ "f"; "f90"; "f95"; "f03"; "f08"; "F"; "F90" ];
    transform_output = Fun.id;
    parallel = true;
    wrap_c_flags = Fun.id;
  }

let default = [ clang; clangxx; ispc; ghc; mlton; ats2; flang; ocaml ]
let all = ref default

let compile_obj t ~env ~sw ~output ~checker ~log_level ~build_dir ~build_mtime
    ~objects flags =
  let st =
    try
      Option.some
        ( Eio.Path.stat ~follow:true output.Object_file.path,
          Eio.Path.stat ~follow:true output.source.path )
    with _ -> None
  in
  let src_path =
    if not (Util.is_verbose log_level) then
      Util.truncate_path_left output.source.path
    else Eio.Path.native_exn output.source.path
  in
  let obj_path =
    if not (Util.is_verbose log_level) then
      Util.truncate_path_left output.Object_file.path
    else Eio.Path.native_exn output.path
  in
  match st with
  | Some (obj, src) when obj.mtime > src.mtime && obj.mtime > build_mtime ->
      Util.log_spinner
        ~verbose:(Util.is_verbose log_level)
        "CACHE %s -> %s" src_path obj_path;
      None
  | _ ->
      Util.log_spinner
        ~verbose:(Util.is_verbose log_level)
        "COMPILE(%s) %s -> %s" t.name src_path obj_path;
      Util.mkparent output.Object_file.path;
      let cmd = t.command ~flags ~output ~objects in
      Command.check_command checker t.name;
      if log_level = `Debug then Util.log "  $ %s" (String.concat " " cmd);
      Log_file.with_log_file ~keep:true ~build_dir
        ~name:(Digest.to_hex (Digest.string (String.concat " " cmd)))
      @@ fun (tmp_path, log_file) ->
      let proc =
        Eio.Process.spawn env#process_mgr cmd ~sw ~stdout:log_file
          ~stderr:log_file
      in
      Some (tmp_path, proc)

let register compiler =
  if not (List.exists (fun c -> c.name = compiler.name) !all) then
    all := compiler :: !all

let find_by_name ?compilers c =
  match
    List.find_opt (fun x -> x.name = c) (Option.value ~default:!all compilers)
  with
  | Some x -> Some x
  | None -> (
      match String.lowercase_ascii c with
      | "c" | "cc" | "clang" -> Some clang
      | "clang++" | "c++" | "cxx" | "cpp" -> Some clangxx
      | "ispc" -> Some ispc
      | "ghc" | "hs" | "lhs" -> Some ghc
      | "flang-new" | "flang" | "fortran" -> Some flang
      | "sml" | "mlton" -> Some mlton
      | "ats" | "ats2" | "pats" | "patscc" -> Some ats2
      | "ocaml" | "ml" | "ocamlopt" | "ocamlfind" -> Some ocaml
      | _ -> None)

module Set = struct
  include Set.Make (struct
    type nonrec t = t

    let compare a b = String_set.compare a.ext b.ext
  end)

  let default = of_list default
end
