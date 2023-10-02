(lang dune 3.8)

(name forthish)

(generate_opam_files true)

(authors "Vasyl Bodnar")

(maintainers "Vasyl-Bodnar")

(license MIT)

(package
 (name forthish)
 (synopsis "Forthish Language Interpreter")
 (description "Interpreter for a Custom Forth-inspired Language")
 (depends ocaml dune)
 (tags
  (language interpreter forthish)))
