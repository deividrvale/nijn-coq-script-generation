(lang dune 3.5)

(using menhir 2.1)

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
  (ocaml  (>= 4.14.0))
  (dune   (>= 3.6.1))
  (menhir (>= 20220210)))
 (tags
  (coq "termination checking" "proof generation")))
