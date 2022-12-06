(lang dune 3.5)

(using menhir 2.1)

(name onijn)

(version 0.1.0)

(generate_opam_files true)

(source
 (github deividrvale/nijn-coq-script-generation))

(authors "Deivid Vale" "Niels van der Weide")

(maintainers "Deivid Vale")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name onijn)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
