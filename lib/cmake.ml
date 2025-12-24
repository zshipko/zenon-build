type t = {
  fmt: Format.formatter;
  buffer: Buffer.t;
}

let v ~project_name () =
  let buffer = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buffer in
  Fmt.pf fmt "cmake_minimum_required(VERSION 3.10)@." ;
  Fmt.pf fmt "project(%s)@.@." project_name;
  { buffer; fmt }

let contents t =
  Format.pp_print_flush t.fmt ();
  Buffer.contents t.buffer

let add_library t ?(shared=false) name files =
  Fmt.pf t.fmt "add_library(%s %a)@." name (Fmt.list ~sep:(Fmt.any " ") Fmt.string) files;
  if shared then Fmt.pf t.fmt "set_target_properties(%s PROPERTIES TYPE SHARED)@." name

let add_executable t ?(link=[]) name files =
  Fmt.pf t.fmt "add_executable(%s %a)@." name (Fmt.list ~sep:(Fmt.any " ") Fmt.string) files;
  if not (List.is_empty link) then
    Fmt.pf t.fmt "target_link_libraries(%s %a)@." name (Fmt.list ~sep:(Fmt.any " ") Fmt.string) link

let add_compile_definitions t name flags =
  if not (List.is_empty flags) then
    Fmt.pf t.fmt "target_compile_definitions(%s PRIVATE %a)@." name (Fmt.list ~sep:(Fmt.any " ") Fmt.string) flags

let target_include_directories t name dirs =
  if not (List.is_empty dirs) then
    Fmt.pf t.fmt "target_include_directories(%s PRIVATE %a)@." name (Fmt.list ~sep:(Fmt.any " ") Fmt.string) dirs

