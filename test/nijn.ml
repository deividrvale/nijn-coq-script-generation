(* open Data.Input_file *)
open File.Wanda
open Syntax.Ty.SType
open Syntax.Term
open File.Proof_script

(* Hardcoding map system for testing purposes *)
(* Declaration of sort names *)
let n_sort = sort_register "nat"
let l_sort = sort_register "list"
let sort_list = [l_sort; n_sort]
let n_ty = base_ty_mk n_sort
let l_ty = base_ty_mk l_sort
let suc_ty = Arrow(n_ty, n_ty)
let nil_ty = l_ty
let cons_ty = Arrow (n_ty, Arrow (l_ty, l_ty))
let map_ty = Arrow(Arrow(n_ty, n_ty), Arrow(l_ty, l_ty))
let map_ty = arr_ty_mk (arr_ty_mk (arr_ty_mk l_ty l_ty) n_ty) (arr_ty_mk l_ty l_ty)

(* let () =
  print_endline (ty_to_string map_ty) *)


let fn_list = [
  (* fn_register "0";
  fn_register "s"; *)
  fn_register "nil";
  fn_register "cons";
  fn_register "map"
]


let () =
  (* fn_register_ty (List.nth fn_list 0) n_ty; *)
  (* fn_register_ty (List.nth fn_list 1) suc_ty; *)
  fn_register_ty (List.nth fn_list 0) nil_ty;
  fn_register_ty (List.nth fn_list 1) cons_ty;
  fn_register_ty (List.nth fn_list 2) map_ty



let () =
  (* Imports and Scope *)
  print_newline ();
  print_endline (import ["Nijn.Nijn"; "Deivid"] );
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
  print_endline (fn_abrv fn_list)

let x = var_register "x"
let y = var_register "y"

let var_list = [x;y]

let x' = var_from_string "x"

let () =
  print_endline (
    String.concat ";" (
      List.map var_to_string (
        Syntax.Term.var_list ()
        )
    )
  )
