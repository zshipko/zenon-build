(lang dune 3.20)

(name zenon)

(generate_opam_files true)

(source
 (codeberg zshipko/zenon))

(authors "Zach Shipko <zachshipko@gmail.com>")

(maintainers "Zach Shipko <zachshipko@gmail.com>")

(license ISC)

(documentation https://codeberg.org/zshipko/zenon)

(package
 (name zenon)
 (synopsis "Zenon: C build tool of the 21st century")
 (description "A 'smart' build tool for C-like languages")
 (depends
  ocaml
  eio_posix
  yaml
  re
  ocamlgraph
  ppx_deriving_yaml
  cmdliner
  ezjsonm
  progress)
 (tags (build)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
