(** This module provide specialized functionality for the generation of Coq proof scripts.

After parsing the output from a termination tool, {b onijn}
keeps all the information about the term rewriting system given as input.

The data needed to generate a proof script is therefore:

- Preamble declarations.
- The list of sorts.
- The list of symbols and their respective arities.
- The declaration of each rule in the system.
- The associative list of interpretation for each function symbol.

Notice that up to the current version,
{b onijn} only produces proof scripts for polynomial interpretations.
More functionality (adding support for more constructs)
will be added when more termination techniques are supported.

*)

(** {1 Preamble Constructions} *)

type import = string list

type scope = string list

val import : import -> string

val scope : scope -> string

(** {1 Sorts and Types} *)

type sort_dec = Syntax.Ty.Sort.t list
(** Sort declaration is a list of sorts. *)

val sort_def_stm : sort_dec -> string
(** [sort_def_stm sdec] produces the coq string defining
    the inductive type for sorts. *)

val sort_abrv : sort_dec -> string
(** [sort_abrv sdec] produces abbreviations for each sort.
    So the proof script becomes more human readable.
*)

val dec_eq_ty : string
(** [dec_eq_ty] is the statement of decidable equality for types. *)

(** {1 Function Symbols and arity} *)

type fn_dec = Syntax.Term.fn list
(** The {b signature} (in rewriting nomenclature) is
    the list (or set) of each funciton symbol present in the TRS.
*)

val fn_def_stm : fn_dec -> string
(** [fn_def_stm fn_dec] is the Coq statement defining each function symbol in [fn_dec]. *)

val arity_def_stm : fn_dec -> string
(** [arity_def_stm fn_dec] is the Coq statement defining the arity, of each function symbol in [fn_dec]. *)

val fn_abrv : fn_dec -> string
(** [arity_def_stm fn_dec] abbreviates each fn_name in [fn_dec] and introduce them as a list of Definition statements in the proof script. *)

val dec_eq_fn : string
(** [dec_eq_fn] is the Coq proof body that equality of  *)

(** {1 Rules and Rewriting} *)

val rules_def_stm : Syntax.Rule.trs -> string
(** [rules_def_stm trs] is the Coq statement declaring each
    rewriting rule in the term rewriting system [trs]. *)

val afs_df_stm : Syntax.Rule.trs -> string -> string
(** [afs_df_stm trs] is the Coq statement declaring the term rewriting system [trs] as a list of rules.*)

(** {1 Polynomial Interpretations} *)

val itp_def_stm :
  (Syntax.Term.fn * Syntax.Poly.poly_fun) list -> string -> string
(** [itp_def_stm itp trs_name]
    is the Coq statement declaring the interpretation of each funciton symbol in the the signature.
    The TRS is declared with the name [trs_name]. *)

(** {1 Termination Statement} *)

val sn_def_stm : string -> string
(** [sn_def_stm trs_name] is the Coq statement that the TRS [trs_name] is strongly normalizing.

This is the final step to build the proof script.

*)
