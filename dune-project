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
 (description "A build tool for compilers supporting separate compilation")
 (depends
  (ocaml
   (>= 5.3.0))
  eio_posix
  yaml
  re
  ocamlgraph
  ppx_deriving_yaml
  cmdliner
  ezjsonm)
 (tags (build)))
