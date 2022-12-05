(** {0 Coq grammar specification}
*)

type keyword =
| Require | Import | Open    | Scope
| Proof   | Qed    | Defined | Definition
| Match   | With   | End | Let | In
| Global  | Instance | Progam | Inductive

val keyword_to_string : keyword -> string
(** [keyword_to_string k] is the written representation
    of [k] as in Coq syntax. *)

val ident_vbar : string -> (string * string * string) list -> string
(** [ident_vbar idt cs ] returns a string in the grammar:

    {b [idt] | <lhs_0> <token> <rhs_0> ... [idt] | <lhs_n> <token> <rhs_n>},

    where [idt] is the identation string and
    the {b <lhs_i> <token> <rhs_i> } are in [cs].
 *)

val cmd_def : keyword -> string -> string -> string
(**  *)

val cmd_stm : ?keyword_list:keyword list -> keyword -> string -> string
(** *)

val cmd_proof : keyword -> string -> string
(** *)

val cmd_ind_dec : keyword -> string -> (string * string * string) list -> string
(**  *)

val match_cmd : string -> string -> string

val let_cmd : string -> string -> string
