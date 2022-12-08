(** This module provides functionality for the notion of terms. *)


(** {1 Function Symbols } *)
open Ty.SType

type fn
(** Abstract type for function symbols. *)

val fn_equal : fn -> fn -> bool
(**
    [fn_equal f g] is whether [f] and [g] is structuraly equal
*)

val fn_list : unit -> fn list
(**
    [fn_list] returns the list of all function symbols names
*)

val get_fn : string -> fn
(**
    [fn_from_string name] is [f] if [f] is registered with name [name].

    @raise [Not_found] if there is no such [f].
*)

val get_fn_opt : string -> fn option
(**
    [get_fn_opt name] is [Some f] if [f] is registered with name [name],
    [None] otherwise.
*)

val fn_register : string -> fn
(**
    [fn_register f] returns a [fn] with name [f].

    {b Side Effect:} [f] is kept in a stack internally.

    @raise DuplicatedName if [fn_register f] is called
        for a name [f] that is already in the stack.
*)

val fn_to_string : fn -> string
(**
    [fn_to_string f] is the string (name) associated with [f].
*)

val fn_compare : fn -> fn -> int
(**
    [fn_compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y],
    and a positive integer if [x] is greater than [y].
*)

val fn_register_ty : fn -> ty -> unit
(**
    [fn_register_ty f t] adds the pair [(f, t)] to the stack.

    {b Side effect:} the list of such pairs [(f,t)]
    is kept as state in the terms module.
*)

val arity : fn -> ty
(**
    [arity f] returns the registered type of [f].

    @raise Name_not_found if no type is registered for [f].
*)

(** {1 Variables } *)

type var
(** Abstract type for variables. *)

val var_equal : var -> var -> bool
(** [var_equal x y] is wheter [x] is structurally equal to [y]. *)

val var_list : unit -> var list
(** [var_list ()] returns the list of all variables registered up to the time
    this function is called. *)

val get_var : string -> var
(** [get_var name] is the variable [v] that is registered with the key name [name].

    @raise Name_not_found if no variable is registered by [name].
*)

val get_var_opt : string -> var option
(** [get_var_opt name] is [Some v] if [v] is a variable registered with
    name key [name], and [None] otherwise. *)


val var_register : string -> var
(** [var_register name] registers a variable [v] using the key name [name].
    If such variable is already registered, then no name is registered and the previously
    recorded name is returned. *)

val var_to_string : var -> string
(** [var_to_string x] is the written representation of the key value
    used to register [x]. *)

(** {1 Terms}
    Finally, given that we have types and functionalities for function symbols
    and variables, we can construct our type for terms.

    Two notions of terms is defined: the type {!type:term} is for named terms,
    i.e., variables have names.
    The second type {!type:bruijn} is an abstract type used to represent terms
    in de bruijn notation.
    We instantiate {!type:bruijn} to a concrete implementation {!type:nameless},
    where function symbols are {!type:fn} and indeces are integers.

*)

(** {2 AFS Terms} *)
(*-----------------------------------------------------------------------------
  API for Terms
-----------------------------------------------------------------------------*)

type term =
  | Fun of fn
  | Var of var
  | Lam of var * term
  | App of term * term
(** Type for terms. *)

val tm_to_string : term -> string
(** [tm_to_string t] is the written representation of [t].
    It uses standard rules to generate the strings,
    so unecessary parenthesis are removed:
    - application is left associative
    - application has precedence over abstraction
    - abstraction is right-associative *)

val free_var : term -> var list
(** [free_var t] return the list of free variables in [t] *)

val term_equal : term -> term -> bool
(** [term_equal t t'] is whether [t] and [t'] are equal
    module alpha-equivalence. *)

(** {2 de Bruijn Terms}
    In de Bruijn notation, variables are nameless,
    i.e., they are represented by numbers.

    In a closed term, the indexes represent the distance of
    a variable to its binding abstractor.
    If the original term [t] has free variables,
    then we need a naming context to represent [t]
    as a nameless term.

*)

(*-----------------------------------------------------------------------------
    de Bruijn Terms
-----------------------------------------------------------------------------*)

type ('f, 'v) bruijn =
    | NFun of 'f
    | NVar of 'v
    | NLam of ('f, 'v) bruijn
    | NApp of ('f, 'v) bruijn * ('f, 'v) bruijn
(** Abstract type for de Bruijn term representation. *)


type nameless = (fn, int) bruijn
(** Concrete instantiation of {!type:bruijn} type.
Function symbols are {!type:fn} and variables (indices)
are integers. *)

val terms_to_bruijn : term -> nameless
(** [terms_to_bruijn t] returns the nameless representation of [t],
    it uses the set of free-variables of [t] as the name context
    to determine the index of free variables.  *)

val terms_to_bruijn_ctx : var list -> term -> nameless
(** [terms_to_bruijn_ctx ctx t] returns the nameless representation of [t];
    but contrary to {!val:terms_to_bruijn},
    it uses the context [ctx] to determine the position of the free variables
    in [t].
 *)

val nameless_to_string : nameless -> string
(** [nameless_to_string n] returns the string representation of the nameless
    term [n]. *)

val nameless_equal : nameless -> nameless -> bool
(** [nameless_equal n n'] is whether [n] and [n'] are syntactically equal.

    {b Note:} syntactical equality of two nameless terms imply in alpha-equality.
    Therefore, if [t,t'] are values of type {!type:term},
    then [t] is alpha-equivalent to [t'] iff

    [nameless_equal (terms_to_bruijn t) (terms_to_bruijn t') ]

    evaluates to [true].
*)
