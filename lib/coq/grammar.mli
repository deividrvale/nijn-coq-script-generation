(** This module provides functionality {b onijn}'s "compiler".
    We need to generate string constructs that can be parsed by Coq
    in order to generate a proof-script that can be checked.
    The functions in this module produce strings that satisfy the requirements in
    {{:https://coq.inria.fr/distrib/current/refman/language/core/definitions.html}}.

    Notice that it is possible to produce coq code that doesn't compile in Coq.
    The production functions here only receive strings as input and produce
    strings matching Coq's grammar.
    Coq is the ultimate checker of the output of proof scripts.
*)

(** {1 Keywords} *)

type keyword =
| Require | Import | Open    | Scope
| Proof   | Qed    | Defined | Definition
| Match   | With   | End | Let | In
| Global  | Instance | Progam | Inductive
(** The [keyword] type enumerates the keywords we support. *)

val keyword_to_string : keyword -> string
(** [keyword_to_string k] is the written representation
    of [k] as in Coq syntax. *)

(** {1 Grammar Constructions} *)

val ident_vbar : string -> (string * string * string) list -> string
(** [ident_vbar idt cs ] returns a string in the grammar:

    {b [idt] | <lhs_0> <token> <rhs_0> ... [idt] | <lhs_n> <token> <rhs_n>},

    where [idt] is the identation string and
    the {b <lhs_i> <token> <rhs_i> } are in [cs].
 *)

val cmd_def : keyword -> string -> string -> string
(** [cmd_def keyword ident body] returns a string in the grammar:

    {b
    <keyword> <ident> :=}

    {b <body> . }

    It can be used to define the identifier [ident] with
    the body [body].
*)

val cmd_stm : ?keyword_list:keyword list -> keyword -> string -> string
(** [cmd_stm ~keyword_list:[ks] keyword body] returns a string in the grammar

    {b <[ks]>? <[keyword]> <[body]>.}

    {b Example:} to produce the coq code
    [Require Import Nijn.],
    we invoke [cmd_stm] as follows:

    [cmd_stm ~keyword_list[Require] Import "Nijn"].

    Notice that the optional argument [~keyword_list]
    is needed whenever the coq statement utilizes
    more than one keyword.
*)

val cmd_proof : keyword -> string -> string
(** [cmd_proof keyword body] returns a string in the grammar

    {b Proof.}

    {b <proof_body>}

    {b [keyword].}

    The [keyword] argument should be a valid proof ending
    Coq keyword (if you want to produce valid coq scripting).
    In this version: {!constructor:Qed} or {!constructor:Defined}.
*)

val cmd_ind_dec : keyword -> string -> (string * string * string) list -> string
(** [cmd_ind_dec keyword ident ls] returns a string in the grammar

    {b [keyword] [ident] :=}

    {b |<lhs_0> <token> <rhs_0>}

    {b ... }

    {b | <lhs_n> <token> <rhs_n>.}

    The list [ls] contains the list of triples representing
    the <lhs> <token> <rhs> pattern.
*)

val match_cmd : string -> string -> string
(** [match_cmd match_ident body] returns a string in the grammar

    {b match <[match_ident]> with}

    {b <[body]>}

    {b End}
*)


val let_cmd : string -> string -> string
(** [let_cmd let_ident body] returns a string in the grammar

    {b let <[let_ident]> := <[body]> in}

*)
