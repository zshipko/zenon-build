open Types

type visited = (string, unit) Hashtbl.t
type process = Process : 'a Eio.Process.t -> process
type compile_process = path * process

open Effect
open! Effect.Deep

exception Build_failed of string

type _ Effect.t +=
  | Script : { script : string } -> unit t
  | External : { ext : Build.External.t } -> unit t
  | Compile : {
      compiler : Compiler.t;
      output : Object_file.t;
      objects : Object_file.t Seq.t;
      flags : Flags.t;
    }
      -> compile_process option t
  | Link : {
      output : path;
      linker : Linker.t;
      objects : Object_file.t list;
      flags : Flags.t;
    }
      -> unit t

let compile ~compiler ~output ~objects ~flags =
  Effect.perform (Compile { compiler; output; objects; flags })

let script script = Effect.perform (Script { script })
let extern ext = Effect.perform (External { ext })

let link ~output ~linker ~objects ~flags =
  Effect.perform (Link { output; linker; objects; flags })

let run_script mgr ~build_dir s =
  let cmd = [ "sh"; "-c"; s ] in
  Log_file.with_log_file ~build_dir ~name:(Digest.to_hex (Digest.string s))
  @@ fun (tmp_path, log_file) ->
  try Eio.Process.run mgr cmd ~stdout:log_file ~stderr:log_file
  with exn ->
    (try
       let log = Log_file.get tmp_path in
       Util.log_clear "❌ script failed %s\n%s" s log
     with _ -> ());
    raise exn

let run_external (e : Build.External.t) executed =
  let zenon_bin = Sys.executable_name in
  let start = Unix.gettimeofday () in
  Util.log_clear "◎ %s:%s" e.path e.target;
  let name =
    Digest.to_hex @@ Digest.string @@ Format.sprintf "%s:%s" e.path e.target
  in
  Log_file.with_log_file ~build_dir:e.Build.External.build.Build.build ~name
  @@ fun (tmp_file, log_file) ->
  try
    Eio.Process.run e.Build.External.build.Build.env#process_mgr
      ~stderr:log_file ~stdout:log_file
      ~cwd:Eio.Path.(e.Build.External.build.Build.env#fs / e.path)
      [ zenon_bin; "build"; e.target ];
    Util.log_clear "✓ %s:%s (%fsec)" e.path e.target
      (Unix.gettimeofday () -. start);
    Hashtbl.add executed (Node.node_id (External e)) ()
  with _exn ->
    let log = Log_file.get tmp_file in
    Util.log_clear "%s" log;
    raise (Build_failed "")

let handle (build : Build.t) ~visited ~sw f =
  try f () with
  | effect Script { script }, k ->
      Util.log ~verbose:(Util.is_verbose build.log_level) "• SCRIPT %s" script;
      run_script build.Build.env#process_mgr ~build_dir:build.build script;
      continue k ()
  | effect External { ext }, k ->
      run_external ext visited;
      continue k ()
  | effect Compile { compiler; output; objects; flags }, k ->
      let obj =
        Compiler.compile_obj compiler ~output ~sw ~log_level:build.log_level
          ~build_dir:build.build ~build_mtime:build.mtime ~env:build.env
          ~objects flags
      in
      let obj = Option.map (fun (a, b) -> (a, Process b)) obj in
      continue k obj
  | effect Link { output; linker; objects; flags }, k ->
      (if Util.is_debug build.log_level then
         let link_cmd = linker.command ~flags ~objs:objects ~output in
         Util.log "  $ %s" (String.concat " " link_cmd));
      Linker.link linker build.env#process_mgr ~objs:objects ~output ~flags
        ~build_dir:build.build;
      continue k ()

let wait_process (log_path, Process process) =
  Eio.Process.await_exn process;
  Eio.Path.unlink log_path

let build_error (b : Build.t) (obj : Object_file.t) ~cmd ~exn (log_path, _) =
  let log = Log_file.get ~unlink:true log_path in
  let filepath = Util.relative_to b.source obj.source.path in
  Util.log_error ~log_output:log ~filepath ~target:b.name
    ~command:(String.concat " " cmd) ~exn ();
  raise (Build_failed b.name)
