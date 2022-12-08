(** This module exports a module type {!module-type:NAME}, which is an abstract
    interface for {b names},
    and one module {!module:IndexedName} of type {!module-type:NAME}.
*)

module type NAME = sig
    type t
    (** [t] is the type for {b names}. *)

    val equal : t -> t -> bool
    (** [equal t t'] is whether the names [t] and [t'] are structural equal. *)

    val symb_list : unit -> t list
    (** [symb_list ()] is the list of all names registered up to calling
        this function. *)

    val get_symb : string -> t
    (** [get_symb name] returns the symbol registered with [name].
        @raise Name_Not_found if there is no such name. *)

    val get_symb_opt : string -> t option
    (** [get_symb_opt name] is [Some t] if there is a symbol [t]
        registered with [name], [None] otherwise. *)

    val register_name : string -> t
    (** [register_name name] is the name (of type {!type:t}) registered with key [name].
        If there is already a name registered with this key,
        then [register_name name] returns the already registered name associated
        with this key.
        This behavior make this module behave as a 'set-like' structure. *)

    val to_string : t -> string
    (** [to_string name] is the string representation of the name [name].

        {b Note:} a standard implementation of [to_string] would return the
        same string used as key for registering [name].
        Hence, satisfying
        [to_string (get_symb (register_name "n"))]
        equals ["n"].
        This property is not enforced, however.
        It is up to implementers the choice of how to write names back to string format.
    *)

    val compare : t -> t -> int
    (** [compare x y] returns [0] if [x] is equal to [y],
        a negative integer if [x] is less than [y],
        and a positive integer if [x] is greater than [y].

        {b Note:} [compare] enforces a total order over the type {!type:t}.
    *)

    exception Name_Not_found of string
    (** Raised whenever calling [get_symb]. *)
end
(** Interface for {b names}.
    It can be used to represent base types, variables, and function symbols.*)

module IndexedName : functor () -> NAME
