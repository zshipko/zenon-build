open Cmdliner

let path =
  let doc = "Root directory" in
  Arg.(
    value & opt string (Sys.getcwd ()) & info [ "p"; "path" ] ~doc ~docv:"PATH")

let output =
  let doc = "Output file name" in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv:"PATH")

let targets =
  let doc = "Selected targets" in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"TARGETS")

let cflag =
  let doc = "Compiler flag" in
  Arg.(value & opt_all string [] & info [ "cflag"; "c" ] ~doc ~docv:"FLAG")

let ldflag =
  let doc = "Linker flag" in
  Arg.(value & opt_all string [] & info [ "lflag"; "l" ] ~doc ~docv:"FLAG")

let ignore =
  let doc = "Ignore file" in
  Arg.(value & opt_all string [] & info [ "ignore"; "i" ] ~doc ~docv:"FILE")

let file =
  let doc = "Add file" in
  Arg.(value & opt_all string [] & info [ "file"; "f" ] ~doc ~docv:"FILE")

let run =
  let doc = "Run after compiling" in
  Arg.(value & flag & info [ "run"; "r" ] ~doc)

let arg =
  let doc = "Run argument" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc ~docv:"ARGUMENT")

let pkg =
  let doc = "Pkg-config package" in
  Arg.(value & opt_all string [] & info [ "pkg" ] ~doc ~docv:"PACKAGE")

let linker =
  let doc = "Linker name" in
  Arg.(value & opt (some string) None & info [ "linker" ] ~doc ~docv:"LINKER")

let verbose =
  let doc = "Control log verbosity (-v for verbose, -vv for debug logging)" in
  Arg.(value & flag_all & info [ "verbose"; "v" ] ~doc)

let target =
  let doc = "Target to run" in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"TARGET")

let build =
  let doc = "Compile before running" in
  Arg.(value & flag & info [ "build"; "b" ] ~doc)

let clean_build =
  let doc = "Clean before building" in
  Arg.(value & flag & info [ "clean" ] ~doc)

let run_args =
  let doc = "Arguments to pass to executable" in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let prefix =
  let doc = "Installation prefix" in
  Arg.(value & opt string "/usr/local" & info [ "prefix" ] ~doc ~docv:"PATH")

let version =
  let doc = "Version" in
  Arg.(value & opt string "0.0.0" & info [ "version" ] ~doc ~docv:"VERSION")

let uninstall =
  let doc = "Uninstall artifacts instead of installing" in
  Arg.(value & flag & info [ "u"; "uninstall" ] ~doc)
