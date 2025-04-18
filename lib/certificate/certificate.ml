module Tm  = Syntax.Term
module Pol = Syntax.Poly
module TRS = Syntax.Rule

(*
Reminder: this module is not the most general it can ben since
this datatype is specialized to certificates using rule removal only with
poly interpretations.
For now this is not an issue since that's the only options we have in the
formalization.
*)

exception WrongData of string

type poly_int = (Tm.fn * Pol.poly_fun) list
type poly_rr =  (int list * poly_int ) list

type certificate =
  | EMPTY
  | Poly of poly_int
  | Rem  of poly_rr


(* The data needed to build the file. *)
type cert_data = {
  trs  : TRS.trs ;
  cert : certificate
}
