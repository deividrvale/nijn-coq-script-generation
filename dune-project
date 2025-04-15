(lang dune 3.18)

(using menhir 3.0)

(name onijn)

(version 1.0.0)

(generate_opam_files true)

(source
 (github deividrvale/nijn-coq-script-generation))

(authors "Deivid Vale" "Niels van der Weide")

(maintainers "Deivid Vale")

(license MIT)

(documentation https://deividrvale.github.io/nijn-coq-script-generation/onijn/index.html)

(package
 (name onijn)
 (synopsis "Coq script genaration from proof trace.")
 (description "OCaml module of Nijn to generate coq scripts for checking termination proofs of higher-order rewriting systems.")
 (depends
  (ocaml  (>= 5.3.0))
  (dune   (>= 3.18.0))
  (menhir (>= 20240715)))
 (tags
  (coq "termination checking" "proof generation")))
