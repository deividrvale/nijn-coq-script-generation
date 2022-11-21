open Ty.SType
(*-----------------------------------------------------------------------------
  Function Symbols
-----------------------------------------------------------------------------*)

module FSym = Symb.IndexedName()
type fn = FSym.t

let fn_equal = FSym.equal
let fn_list = FSym.symb_list
let fn_from_string = FSym.get_symb_opt
let fn_register = FSym.register_name
let fn_to_string = FSym.to_string
let fn_compare = FSym.compare

(* The term module keeps track of the the arity
   arity_assoc is a reference pointer to an association list
   of
*)
let arity_assoc : (fn * ty) list ref = ref []

let fn_register_ty fn ty =
  arity_assoc := (fn , ty) :: !arity_assoc

let arity fn =
  let p = (fun (f, _) -> fn_equal fn f) in
  List.find p !arity_assoc
  |> snd

(*-----------------------------------------------------------------------------
  Variable Symbols
-----------------------------------------------------------------------------*)
module VSym = Symb.IndexedName()

type var = VSym.t

let var_equal = VSym.equal

let var_list = VSym.symb_list

let var_from_string = VSym.get_symb_opt

let var_register = VSym.register_name

let var_to_string = VSym.to_string

(* let get_var *)

(*-----------------------------------------------------------------------------
  Terms
-----------------------------------------------------------------------------*)
type term =
  | Fun of fn
  | Var of var
  | Lam of var * term
  | App of term * term

let free_var t =
  let rec fvar_acc = (fun t vs ->
    match t with
    | Fun _ -> []
    | Var v -> v :: vs
    | Lam (v, t) -> Utils.List.remove var_equal v (fvar_acc t vs)
    | App (s,t) -> (fvar_acc s vs) @ (fvar_acc t vs)
    )
  in fvar_acc t []


(* to_string *)
let rec tm_to_string' (b : bool) (t : term) =
  match t with
  | Fun f -> fn_to_string f
  | Var x -> var_to_string x
  | e -> (
    if b then
      print_app e
    else
      "(" ^ print_lambda e ^ ")"
  )
and print_app = function
  | e -> print_other_app e
and print_other_app f =
  match f with
  | App (f, x) -> print_app f ^ " · " ^ (tm_to_string' false x)
  | f -> tm_to_string' false f
and print_lambda = function
  | Lam (v, t) ->
    "λ" ^ var_to_string v ^ "." ^ print_lambda t
  | e -> print_app e

let tm_to_string = tm_to_string' true

(*-----------------------------------------------------------------------------
    de Bruijn Terms
-----------------------------------------------------------------------------*)
type ('f, 'v) bruijn =
    | Fun of 'f
    | Var of 'v
    | Lam of ('f, 'v) bruijn
    | App of ('f, 'v) bruijn * ('f, 'v) bruijn

type nameless = (fn, int) bruijn

let terms_to_bruijn t =
  let rec tm_brj_acc = (fun (tm : term) fvars ->
      match tm with
      | Fun f -> Fun f
      | Var v -> Var (Utils.List.index_of var_equal v fvars)
      | Lam (v, t) -> Lam (tm_brj_acc t (v :: fvars))
      | App (s, t) -> App ((tm_brj_acc s fvars), (tm_brj_acc t fvars))
    )
  in tm_brj_acc t (free_var t)

