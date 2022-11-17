(*-----------------------------------------------------------------------------
  Function Symbols
-----------------------------------------------------------------------------*)
module FSym = Symb.IndexedName()
type fn = FSym.t

let fn_equal = FSym.equal
let fn_list () = FSym.symb_list
let fn_register = FSym.register_name
let fn_to_string = FSym.to_string
let fn_compare = FSym.compare





(*-----------------------------------------------------------------------------
  Variable Symbols
-----------------------------------------------------------------------------*)
module VSym = Symb.IndexedName()
type var = VSym.t

(*-----------------------------------------------------------------------------
  Terms
-----------------------------------------------------------------------------*)
type term =
  | Fun of fn
  | Var of var
  | Lam of var * term
  | App of term * term
