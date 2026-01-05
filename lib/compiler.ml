open Types

type t = {
  name : string;
  command :
    flags:Flags.t ->
    objects:Object_file.t list ->
    output:Object_file.t ->
    string list;
  ext : String_set.t;
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
  }

let clangxx =
  {
    name = "clang++";
    command = c_like [ "clang++" ];
    ext = String_set.of_list [ "cc"; "cpp"; "cxx" ];
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
  }

let ghc =
  {
    name = "ghc";
    command =
      (fun ~flags ~objects ~output ->
        let hidir =
          match Eio.Path.split output.Object_file.path with
          | None -> assert false
          | Some (p, _) -> [ "-hidir"; Eio.Path.native_exn p ]
        in
        let include_paths =
          List.map
            (fun obj ->
              match Eio.Path.split obj.Object_file.source.path with
              | None -> assert false
              | Some (p, _) -> "-i" ^ Eio.Path.native_exn p)
            objects
        in
        [
          "ghc";
          "-fdiagnostics-color=always";
          "-v0";
          "-c";
          "-o";
          Eio.Path.native_exn output.Object_file.path;
        ]
        @ hidir @ include_paths @ flags.Flags.compile
        @ [ Eio.Path.native_exn output.source.path ]);
    ext = String_set.of_list [ "hs"; "lhs" ];
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
        let cc_opts =
          List.concat_map (fun x -> [ "-cc-opt"; x ]) flags.compile
        in
        let args =
          [ "mlton"; "-stop"; "o"; "-output"; out ] @ cc_opts @ [ src ]
        in
        [
          "sh";
          "-c";
          String.concat " " args
          ^ Printf.sprintf
              " && ld -r -keep_private_externs %s.0.o %s.1.o -o %s && rm \
               %s.0.o %s.1.o"
              out out out out out;
        ]);
    ext = String_set.of_list [ "sml"; "mlb" ];
  }

let ats2 =
  {
    name = "patscc";
    command = c_like [ "patscc"; "-Wno-unused-command-line-argument" ];
    ext = String_set.of_list [ "dats"; "pats" ];
  }

let flang =
  {
    name = "flang-new";
    command = c_like [ "flang-new" ];
    ext = String_set.of_list [ "f"; "f90"; "f95"; "f03"; "f08"; "F"; "F90" ];
  }

let compile_obj t ~env ~sw ~output ~log_level ~build_dir ~build_mtime ~objects
    flags =
  let st =
    try
      Option.some
        ( Eio.Path.stat ~follow:true output.Object_file.path,
          Eio.Path.stat ~follow:true output.source.path )
    with _ -> None
  in
  let src_path = Util.truncate_path_left output.source.path in
  let obj_path = Util.truncate_path_left output.Object_file.path in
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
      if log_level = `Debug then Util.log "  $ %s" (String.concat " " cmd);
      Log_file.with_log_file ~keep:true ~build_dir
        ~name:(Digest.to_hex (Digest.string (String.concat " " cmd)))
      @@ fun (tmp_path, log_file) ->
      let proc =
        Eio.Process.spawn env#process_mgr cmd ~sw ~stdout:log_file
          ~stderr:log_file
      in
      Some (tmp_path, proc)

let default = [ clang; clangxx; ispc; ghc; mlton; ats2; flang ]
let all = ref default

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
      | _ -> None)

module Set = struct
  include Set.Make (struct
    type nonrec t = t

    let compare a b = String_set.compare a.ext b.ext
  end)

  let default = of_list default
end
