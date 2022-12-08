(** This module provides functionalities for representing polynomials. *)

(** {1 Basic Polynomials}
    In order to create a polynomial,
    we need a type for variables and numbers injection (since we have polynomials
    over integers.)
*)

module PolV : Symb.NAME
(** Polynomial variables are collected in a module of type
    {!module-type: Symb.NAME }.
*)

(** {2 The Poly Type}  *)

type name = PolV.t
(** Type alias for poly variable names. *)

type poly
(** Untyped polynomials over int. *)

(** {2 Basic Injection}
    The simplest form of polynomials: numbers and variables.
    The two values below injects the types [int] and [var]
    into the {!type: poly} type.
*)

val num : int -> poly
(** [num i] is the polynomial representing the integer [i]. *)

val var : name -> poly
(** [var x] is the polynomial representing the variable [x]. *)

(** {1 Operations over polynomials } *)

val add : poly -> poly -> poly
(** [add p p'] returns the polynomial [p + p']. *)

val mul : poly -> poly -> poly
(** [mul p p'] returns the polynomial [p * p'] *)

val app : poly -> poly -> poly
(** [app p p'] returns the application of [p] to [p']. *)

val get_vars : poly -> name list
(** [get_vars p] returns the list of variables occurring in [p].*)

val equal : poly -> poly -> bool
(** [equal p p'] returns whether the polynomials [p] and [p'] are syntactically
    equal. *)

val simplify : poly -> poly
(** [simplify p] reduces the polynomial [p] to a summation of monomials.*)

(** {1 Utilities for Poly} *)

val to_string : poly -> string
(** [to_sring p] print the written human-readable representation of the polynomial [p].*)

val var_occurs : poly -> name -> bool
(** [var_occurs p x] returns whether the name [x] occurs in the syntax of [p]. *)

val apply_poly_list : poly -> poly list -> poly
(** [apply_poly_list p ps] is the polynomial [p], if [ps] is empty; and it is the left-associated application of [p] to each element of [ps]. *)

(** {1 Polynomial Functions }
    Polynomial functions are expressions of the form:
    Lam [x1, ..., xn]. P, where [x1, ..., xn] are values
    of type {!type:name} and P is of type {!type:poly}.
    The internal type is abstract.
*)

type poly_fun
(** The type of polynomial functional expressions. *)

val poly_fun_mk : name list -> poly -> poly_fun
(** [poly_fun_mk xs p] returns the polynomial expression representing the polynomial function over variables [xs] and polynomial [p]. *)

val get_names : poly_fun -> name list
(** [get_names pf] returns the list of names occurring in the syntax of [pf].  *)

val get_poly : poly_fun -> poly
(** [get_poly pf] returns the polynomial [p] in the polynomial expression. *)
