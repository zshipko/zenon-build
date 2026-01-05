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

let c_like ?(force_color = "-fcolor-diagnostics") cc =
 fun ~flags ~sources:_ ~output ->
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
      (fun ~flags ~sources:_ ~output ->
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
      (fun ~flags ~sources:_ ~output ->
        let hidir, include_paths =
          match Eio.Path.split output.Object_file.path with
          | None -> ([], [])
          | Some (parent, _) ->
              let dir = Eio.Path.native_exn parent in
              ([ "-hidir"; dir ], [ "-i" ^ dir ])
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
      (fun ~flags ~sources:_ ~output ->
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

let compile_obj t mgr ~sources ~sw ~output ~build_mtime ?(verbose = false)
    ~build_dir flags =
  let st =
    try
      Option.some
        ( Eio.Path.stat ~follow:true output.Object_file.path,
          Eio.Path.stat ~follow:true output.source.path )
    with _ -> None
  in
  match st with
  | Some (obj, src) when obj.mtime > src.mtime && obj.mtime > build_mtime ->
      Util.log_spinner ~verbose "CACHE %s -> %s"
        (Eio.Path.native_exn output.source.path)
        (Eio.Path.native_exn output.Object_file.path);
      None
  | _ ->
      Util.log_spinner ~verbose "COMPILE(%s) %s -> %s" t.name
        (Eio.Path.native_exn output.source.path)
        (Eio.Path.native_exn output.Object_file.path);
      Util.mkparent output.Object_file.path;
      let cmd = t.command ~sources ~flags ~output in
      Log_file.with_log_file ~keep:true ~build_dir
        ~name:(Digest.to_hex (Digest.string (String.concat " " cmd)))
      @@ fun (tmp_path, log_file) ->
      let proc =
        Eio.Process.spawn mgr cmd ~sw ~stdout:log_file ~stderr:log_file
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
