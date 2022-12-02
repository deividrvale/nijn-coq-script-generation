module PolV : Symb.NAME
type name = PolV.t

type poly
(** Untyped polynomials over int. *)

val num : int -> poly

val var : name -> poly

val get_vars : poly -> name list

val add : poly -> poly -> poly

val mul : poly -> poly -> poly

val app : poly -> poly -> poly

val equal : poly -> poly -> bool

val reduce : poly -> poly

val simplify : poly -> poly

val to_string : poly -> string

val var_occurs : poly -> name -> bool

type poly_fun

val poly_fun_mk : name list -> poly -> poly_fun

val get_names : poly_fun -> name list

val get_poly : poly_fun -> poly
