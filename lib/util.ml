let lock = Mutex.create ()
let spinner_frames = [| "◜"; "◝"; "◞"; "◟" |]
let spinner_idx = ref 0

type log_level = [ `Quiet | `Info | `Debug ]

let log_level = function 0 -> `Quiet | 1 -> `Info | _ -> `Debug
let is_verbose = function `Quiet -> false | `Info | `Debug -> true
let is_debug = function `Debug -> true | _ -> false

type progress_state = {
  mutable current : int;
  total : int;
  is_tty : bool;
  mutable current_file : string;
}

let progress : progress_state option Atomic.t = Atomic.make None

let init_progress total =
  if Unix.isatty Unix.stderr && total > 0 then
    Atomic.set progress
      (Some { current = 0; total; is_tty = true; current_file = "" })

let finalize_progress () =
  match Atomic.exchange progress None with
  | Some p when p.is_tty ->
      (* Clear the progress line *)
      Fmt.epr "\r\027[K%!"
  | _ -> ()

let clear_progress_bar () =
  match Atomic.get progress with
  | Some p when p.is_tty ->
      (* Clear current line *)
      Fmt.epr "\r\027[K%!";
      flush stderr
  | _ -> ()

let terminal_width = ref None

let terminal_width ?(force = false) () =
  if force then terminal_width := None;
  match !terminal_width with
  | Some w -> w
  | None ->
      let width =
        match Sys.getenv_opt "COLUMNS" with
        | Some cols -> int_of_string cols
        | None -> (
            try
              let ic = Unix.open_process_in "tput cols" in
              let w = int_of_string (input_line ic) in
              close_in ic;
              w
            with _ -> 80)
      in
      terminal_width := Some width;
      width

let truncate_left ?(max_len = 30) s =
  if String.length s > max_len then
    String.sub s (String.length s - max_len) max_len
  else s

let truncate_path_left ?max_len s =
  let s = Eio.Path.native_exn s in
  truncate_left ?max_len s

let redraw_progress_bar () =
  match Atomic.get progress with
  | Some p when p.is_tty && p.current < p.total && p.current > 0 ->
      (* Don't redraw if we're at 100% or haven't started - it will be finalized soon *)
      let frame =
        spinner_frames.(!spinner_idx mod Array.length spinner_frames)
      in
      let percent = if p.total > 0 then p.current * 100 / p.total else 0 in
      let bar_width = 20 in
      let filled = bar_width * percent / 100 in
      let bar =
        String.concat ""
          (List.init bar_width (fun i -> if i < filled then "█" else "░"))
      in
      (* Calculate available space for filename based on terminal width *)
      let term_width = terminal_width () in
      (* Format without filename to calculate fixed width *)
      let fixed_part =
        Printf.sprintf "%s [%s] %d%% (%d/%d) " frame bar percent p.current
          p.total
      in
      let fixed_width = String.length fixed_part in
      let available_for_file = max 20 (term_width - fixed_width - 1) in
      let file_display =
        truncate_left ~max_len:available_for_file p.current_file
      in
      Fmt.epr "%s%s%!" fixed_part file_display
  | _ -> ()

let log_clear ?(verbose = true) fmt =
  Fmt.kstr
    (fun msg ->
      Mutex.protect lock @@ fun () ->
      if verbose then (
        clear_progress_bar ();
        Fmt.epr "%s@." msg;
        redraw_progress_bar ()))
    fmt

let log ?(verbose = true) fmt =
  Mutex.protect lock @@ fun () ->
  if verbose then Fmt.epr (fmt ^^ "@.") else Fmt.kstr ignore fmt

let log_error ~log_output ~filepath ~target ?command ?exn () =
  Mutex.protect lock @@ fun () ->
  clear_progress_bar ();
  Fmt.epr "\n%s@." log_output;
  Fmt.epr "compilation failed for '%s' in target '%s'@." filepath target;
  let () =
    match command with None -> () | Some cmd -> Fmt.epr "command: %s@." cmd
  in
  let () =
    match exn with Some e -> Fmt.epr "\tmessage: %a@." Fmt.exn e | None -> ()
  in
  redraw_progress_bar ()

let log_spinner ?(verbose = true) fmt =
  if verbose then Mutex.protect lock @@ fun () -> Fmt.epr ("• " ^^ fmt ^^ "@.")
  else
    Fmt.kstr
      (fun msg ->
        Mutex.protect lock @@ fun () ->
        match Atomic.get progress with
        | Some p ->
            p.current <- p.current + 1;
            p.current_file <- msg;
            let frame =
              spinner_frames.(!spinner_idx mod Array.length spinner_frames)
            in
            incr spinner_idx;
            let percent =
              if p.total > 0 then p.current * 100 / p.total else 0
            in
            let bar_width = 25 in
            let filled = bar_width * percent / 100 in
            let bar =
              String.concat ""
                (List.init bar_width (fun i -> if i < filled then "█" else "░"))
            in
            (* Calculate available space for filename based on terminal width *)
            let term_width = terminal_width () in
            let fixed_part =
              Printf.sprintf "\r\027[K%s [%s] %d%% (%d/%d) " frame bar percent
                p.current p.total
            in
            let fixed_width = String.length fixed_part - 5 in
            (* -5 for escape codes *)
            let available_for_file = max 20 (term_width - fixed_width - 1) in
            let file_display = truncate_left ~max_len:available_for_file msg in
            Fmt.epr "%s%s%!" fixed_part file_display
        | None -> ())
      fmt

let ext path =
  let s =
    Eio.Path.split path |> Option.map snd |> Option.value ~default:""
    |> Filename.extension
  in
  if String.length s > 0 then String.sub s 1 (String.length s - 1) else ""

let with_ext path ext =
  let a, b = Eio.Path.split path |> Option.get in
  let c = Filename.remove_extension b ^ "." ^ ext in
  Eio.Path.(a / c)

let mkparent path =
  let parent = Eio.Path.split path |> Option.map fst in
  Option.iter
    (fun p ->
      if Eio.Path.native_exn p = "." || Eio.Path.native_exn p = "" then ()
      else if not (Eio.Path.is_directory p) then
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p)
    parent

let relative_to base a =
  let prefix = Eio.Path.native_exn base in
  let a = Eio.Path.native_exn a in
  if String.starts_with ~prefix a then
    let prefix_len = String.length prefix in
    String.sub a (prefix_len + 1) (String.length a - prefix_len - 1)
  else a

let glob =
  Re.Glob.glob ~pathname:true ~anchored:true ~double_asterisk:true
    ~expand_braces:true

let glob_path path =
  glob @@ Printf.sprintf "{%s,%s}" path @@ Filename.concat "**" path

let is_static_lib (filename : string) =
  String.starts_with ~prefix:"lib" filename
  && String.ends_with ~suffix:".a" filename

let is_shared_lib (filename : string) =
  String.starts_with ~prefix:"lib" filename
  && (String.ends_with ~suffix:".so" filename
     || String.ends_with ~suffix:".dylib" filename)

let normalize_shared_lib_ext (filename : string) =
  if Sys.os_type = "Unix" && String.ends_with ~suffix:".so" filename then
    try
      let ic = Unix.open_process_in "uname -s" in

      let uname =
        Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
        String.trim @@ input_line ic
      in
      if uname = "Darwin" then Filename.remove_extension filename ^ ".dylib"
      else filename
    with _ -> filename
  else filename

let parse_gitignore path =
  if Eio.Path.is_file path then
    Eio.Path.with_lines path @@ fun lines ->
    Seq.filter_map
      (fun line ->
        let line = String.trim line in
        if String.length line = 0 || String.starts_with ~prefix:"#" line then
          None
        else
          let pattern =
            if String.starts_with ~prefix:"/" line then
              String.sub line 1 (String.length line - 1)
            else line
          in
          Some (glob_path pattern))
      lines
    |> List.of_seq
  else []

let extension_is_c_or_cxx = function
  | "c" | "cc" | "cpp" | "cxx" -> true
  | _ -> false

let remove_duplicates_preserving_order lst =
  let seen = Hashtbl.create (List.length lst) in
  let acc = ref [] in
  List.iter
    (fun x ->
      if not (Hashtbl.mem seen x) then (
        Hashtbl.add seen x ();
        acc := x :: !acc))
    lst;
  List.rev !acc

