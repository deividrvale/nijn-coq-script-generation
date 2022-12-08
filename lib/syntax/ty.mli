(** Implementation of {b simple types}.
    This module exports the functor {!module:MakeSType}, a {!module: Sort}
    module of type {!module-type: Symb.NAME},
    and a concrete simple types implementation in {!module: SType}.
*)

(** {1 Abstract Interface}
    The abstract interface for simple types is the functor {!module: MakeSType}.
*)

module MakeSType :
  functor (B : Symb.NAME) ->
    sig
      type sort = B.t
      type ty = Base of sort | Arrow of ty * ty
      val sort_equal : B.t -> B.t -> bool
      val sort_list : unit -> B.t list
      val get_sort : string -> B.t
      val get_sort_opt : string -> B.t option
      val sort_register : string -> B.t
      val sort_to_string : B.t -> string
      val base_ty_mk : sort -> ty
      val arr_ty_mk : ty -> ty -> ty
      val ty_equal : ty -> ty -> bool
      val ty_order : ty -> int
      val is_base : ty -> bool
      val ty_to_string : ty -> string
    end
(** Functor building simple types module.
    It guarantees that simple types use {!module-type: Symb.NAME} for representing
    sorts.
*)

(**
{1 Provided Implementations }

The default implementations for the type of {b sort} and {b simple type}.
*)

module Sort : Symb.NAME
(** The provided sort module. *)

module SType :
  sig
    type sort = Sort.t
    type ty = MakeSType(Sort).ty = Base of sort | Arrow of ty * ty
    val sort_equal : Sort.t -> Sort.t -> bool
    val sort_list : unit -> Sort.t list
    val get_sort : string -> Sort.t
    val get_sort_opt : string -> Sort.t option
    val sort_register : string -> Sort.t
    val sort_to_string : Sort.t -> string
    val base_ty_mk : sort -> ty
    val arr_ty_mk : ty -> ty -> ty
    val ty_equal : ty -> ty -> bool
    val ty_order : ty -> int
    val is_base : ty -> bool
    val ty_to_string : ty -> string
  end
(** Provided simple types module. *)

val make_args_names : SType.ty -> string list
(** [make_args_names t] return a list of names for each argument of a type [t].

    {b Example:} Given a simple type [t1 -> ... -> tn -> b],
    the returned list is [s1, ..., sn].
    Each [si], with [1 <= i <= n],
    is written as exactly ["xi"] or ["Fi"] depending if
    the corresponding type [ti] is a base or arrow type.
*)
