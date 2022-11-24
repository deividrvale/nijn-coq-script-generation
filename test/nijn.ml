(* open Data.Input_file *)
open File.Wanda
(* open Syntax *)
open Syntax.Ty.SType
open Syntax.Term
open Syntax.Poly
open File.Proof_script

(* Hardcoding map system for testing purposes *)
(* Declaration of sort names *)
let n_sort = sort_register "nat"
let l_sort = sort_register "list"
let sort_list = [l_sort; n_sort]
let n_ty = base_ty_mk n_sort
let l_ty = base_ty_mk l_sort
let nil_ty = l_ty
let cons_ty = Arrow (n_ty, Arrow (l_ty, l_ty))
let map_ty = Arrow(Arrow(n_ty, n_ty), Arrow(l_ty, l_ty))

(* let () =
  print_endline (ty_to_string map_ty) *)

let fn_list = [
  (* fn_register "0";
  fn_register "s"; *)
  fn_register "nil";
  fn_register "cons";
  fn_register "map"
]

let nil = List.nth fn_list 0
let cons = List.nth fn_list 1
let map = List.nth fn_list 2

let () =
  fn_register_ty nil nil_ty;
  fn_register_ty cons cons_ty;
  fn_register_ty map map_ty
let f = var_register "f"
let x = var_register "x"
let xs = var_register "xs"
let map_nil = Syntax.Rule.rule_mk
  (App( App(Fun map, Var f), Fun nil))
  (Fun nil)

let map_cons = Syntax.Rule.rule_mk
  (App( App(Fun map, Var f), (App (App (Fun cons, Var x), Var xs )) ))
  (App( App( Fun cons, App(Var f, Var x)), App (App (Fun map, Var f), Var xs)))

let map_trs = [map_nil; map_cons]

let () =
  (* Imports and Scope *)
  print_newline ();
  print_endline (import ["Nijn.Nijn"] );
  print_endline (scope ["poly_scope"]);
  print_newline ();
  (* Sorts *)
  print_endline (sort_def_stm sort_list);
  print_newline ();
  print_endline dec_eq_ty;
  print_newline ();
  print_endline (sort_abrv sort_list);
  print_newline ();
  (* Function Symbols *)
  print_endline (fn_def_stm fn_list);
  print_newline ();
  print_endline dec_eq_fn;
  print_newline ();
  print_endline (arity_def_stm fn_list);;
  print_newline ();
  print_endline (fn_abrv fn_list);
  (* print_endline (gen_ctx 7) *)
  print_endline (rules_def_stm map_trs)

let p_x = Var (FOVar (PolV.register_name "x"))
let p_y = Var (FOVar (PolV.register_name "y"))

let pol1 = Add (App ((HOVar (PolV.register_name "F")), Add (Num 1, Mul (Num 3, Add (Add (p_x, p_y), Num 7)))), p_x)

let s_pol1 = simplify pol1

let () =
  print_endline (Syntax.Poly.to_string pol1 var_to_string);
  print_endline (Syntax.Poly.to_string s_pol1 var_to_string)
