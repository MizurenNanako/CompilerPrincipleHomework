(lang dune 3.15)

(using menhir 3.0)

(name cph)

(source
 (github MizurenNanako/compiler-principle-homework))

(authors "Cherry Sigma")

(maintainers "Cherry Sigma")

(license MIT)

(package
 (name cph)
 (synopsis "cph")
 (description
  "A much simplified c subset compiler example for homework of course .")
 (depends ocaml dune menhirLib))
