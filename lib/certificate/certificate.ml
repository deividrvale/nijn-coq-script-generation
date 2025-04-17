(* A certificate *)
(*

YES

Signature : [<list_of_symbol>]

Rules : [<list_of_rules>]

C = POLY | REM

Certificate(C) = {
<the certficate, for which for each C there is a different parser
}

Ideas:

  - certificate for polynomial interpretations:
  The certificate is the interpretation J.

  - certificate for rule removal:

    - The selector keeps the position of each rule that can be removed at each step:
      - selector is a predicate from: int -> bool
      that selects the rewriting rules that can be oriented.


  It is a sequence of selectors and (polynomial interpretations)

  Base type B
  Signature F,

  Rules : [r0, ...., rk]

  [(selector, )]
*)

type 'a certificate = EMPTY | Poly of 'a | Rem of 'a

let t = Poly 3

let t' = Rem "x"
