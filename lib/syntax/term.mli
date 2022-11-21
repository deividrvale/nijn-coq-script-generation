open Ty.SType
(*-----------------------------------------------------------------------------
  API for Function Symbols
-----------------------------------------------------------------------------*)
type fn
(** Function Symbols  *)

val fn_equal : fn -> fn -> bool
(**
    [fn_equal f g] is whether [f] and [g] is structuraly equal
*)

val fn_list : unit -> fn list
(**
    [fn_list] returns the list of all function symbols names
*)

val fn_from_string : string -> fn option
(**
    [fn_from_string name] is [Some f] if [f] is registered with name [name],
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

    @raise Not_found if no type is registered for [f].
*)

(*-----------------------------------------------------------------------------
  API for Variables
-----------------------------------------------------------------------------*)
type var

val var_equal : var -> var -> bool

val var_list : unit -> var list

val var_from_string : string -> var option

val var_register : string -> var

val var_to_string : var -> string

(*-----------------------------------------------------------------------------
  API for Terms
-----------------------------------------------------------------------------*)
type term =
  | Fun of fn
  | Var of var
  | Lam of var * term
  | App of term * term

val tm_to_string : term -> string

(*-----------------------------------------------------------------------------
    de Bruijn Terms
-----------------------------------------------------------------------------*)

type ('f, 'v) bruijn =
    | Fun of 'f
    | Var of 'v
    | Lam of ('f, 'v) bruijn
    | App of ('f, 'v) bruijn * ('f, 'v) bruijn

type nameless = (fn, int) bruijn

val terms_to_bruijn : term -> nameless
