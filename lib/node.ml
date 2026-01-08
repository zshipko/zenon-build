open Types

type t =
  | Build of Build.t
  | Src of Source_file.t
  | Obj of Object_file.t
  | Output of path
  | External of Build.External.t

let node_id = function
  | Build b -> "build:" ^ b.name
  | Src s -> "src:" ^ Eio.Path.native_exn s.path
  | Obj s -> "obj:" ^ Eio.Path.native_exn s.path
  | Output s -> "out:" ^ Eio.Path.native_exn s
  | External e ->
      "external:" ^ e.Build.External.path ^ ":" ^ e.Build.External.target

type edge =
  | Script of string * path option
  | Compiler of Compiler.t * Flags.t option
  | Linker of Linker.t
  | Dependency

let edge_id = function
  | Script (b, output) ->
      "script:"
      ^ (Digest.string
           ((Option.map Eio.Path.native_exn output |> Option.value ~default:"")
           ^ b)
        |> Digest.to_hex)
  | Compiler (c, Some f) ->
      "compiler:" ^ c.name ^ "_" ^ String.concat "_" f.link
      ^ String.concat "_" f.compile
  | Compiler (c, None) -> "compiler:" ^ c.name
  | Linker link -> "linker:" ^ link.name
  | Dependency -> "dependency"
