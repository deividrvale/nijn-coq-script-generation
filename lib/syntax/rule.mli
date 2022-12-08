(** This module provides functionality to represent rewriting rules. *)

open Term
(** *)

exception Invalid_rule of string
(** Exception raised when trying to create an invalid rewrite rule. *)

type rule
(** Abstract type for rule. *)

type trs = rule list
(** A TRS is a list of {!type:rule} *)

val rule_mk : term -> term -> rule
(**
    [rule_mk l r] creates a rewriting rule with [l] as lhs and [r] as rhs.
    @raise Invalid_rule if [l] and [r] satisfy one of the folowing conditions:
    - [l] is a variable term, i.e., [x == Var _]
    - there is a free variable in [r] that doesn't appear free in [l]
*)

val lhs : rule -> term
(**
    [lhs r] returns the lef-hand-side term of the rewrite rule [r].
*)

val rhs : rule -> term
(**
    [rhs r] returns the right-hand-side term of the rewrite rule [r].
*)

val equal : rule -> rule -> bool
(**
    [equal r r'] is whether [r] is structurally equal to [r'].
    Two rules are equal if their respective lhs (rhs) terms are structurally equal (module alpha-equivalence).
*)

val to_string : rule -> string
(**
    [to_string r] returns a string representation of
    [r].
*)

val get_label : rule -> trs -> string
(**
    [get_label trs] returns the string label of the trs [trs].
*)
