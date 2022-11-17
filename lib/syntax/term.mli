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

(*-----------------------------------------------------------------------------
  API for Variables
-----------------------------------------------------------------------------*)
