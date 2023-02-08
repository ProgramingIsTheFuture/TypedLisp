(lang dune 3.3)

(using menhir 2.1)

(generate_opam_files true)

(name typed_lisp)

(package
  (name typed_lisp)
  (depends ocaml dune llvm))
