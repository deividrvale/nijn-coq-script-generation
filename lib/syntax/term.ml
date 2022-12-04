open Ty.SType
(*-----------------------------------------------------------------------------
  Function Symbols
-----------------------------------------------------------------------------*)

module FSym = Symb.IndexedName ()
type fn = FSym.t

module FnMap = Map.Make (FSym)

let fn_equal = FSym.equal
let fn_list = FSym.symb_list
let get_fn = FSym.get_symb
let get_fn_opt = FSym.get_symb_opt
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
let get_var = VSym.get_symb
let get_var_opt = VSym.get_symb_opt

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
  let rec fvar_acc = (fun t acc ->
    match t with
    | Fun _ -> []
    | Var v -> Utils.Lists.cons_uniq var_equal v acc
    | App (t1,t2) ->
      (fvar_acc t1 acc) @ (fvar_acc t2 acc)
    | Lam (v, t') -> Utils.Lists.remove var_equal v (fvar_acc t' acc)
    )
  in Utils.Lists.remove_duplicates var_equal (fvar_acc t [])

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
    | NFun of 'f
    | NVar of 'v
    | NLam of ('f, 'v) bruijn
    | NApp of ('f, 'v) bruijn * ('f, 'v) bruijn

type nameless = (fn, int) bruijn

let rec terms_to_bruijn_ctx ctx (tm : term) : nameless =
  match tm with
  | Fun f -> NFun f
  | Var v ->
    NVar (Utils.Lists.index_of var_equal v ctx)
  | Lam (v, t) ->
    NLam (terms_to_bruijn_ctx (v :: ctx) t)
  | App (s, t) ->
    NApp (
      (terms_to_bruijn_ctx ctx s),
      (terms_to_bruijn_ctx ctx t)
    )

let terms_to_bruijn t =
  terms_to_bruijn_ctx (free_var t) t

let rec nameless_to_string' (is_root : bool) (t : nameless) =
  match t with
  | NFun f -> fn_to_string f
  | NVar i -> " V " ^ (Int.to_string i)
  | e -> (
    if is_root then
      show_app e
    else
      "(" ^ show_lam e ^ ")"
  )
and show_app = function
  | NApp (f, x) -> show_app f ^ " · " ^ (nameless_to_string' false x)
  | e -> nameless_to_string' false e
and show_lam = function
  | NLam t -> "λ " ^ show_lam t
  | e -> show_app e

let nameless_to_string = nameless_to_string' false

let rec nameless_equal (s : nameless) (t : nameless) : bool =
  match (s,t) with
  | (NVar x, NVar y) -> x == y
  | (NFun x, NFun y) -> fn_equal x y
  | (NApp (s,s'), NApp (t,t')) ->
    (nameless_equal s t) && (nameless_equal s' t')
  | (NLam s, NLam t) -> nameless_equal s t
  | _ -> false

let term_equal (s : term) (t : term) : bool =
  let (nml_s, nml_t) =
    (terms_to_bruijn s, terms_to_bruijn t)
  in nameless_equal nml_s nml_t
