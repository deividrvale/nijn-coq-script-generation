open Syntax.Ty.SType
open Syntax.Term
open CoqGrammar



(*-----------------------------------------------------------------------------
  Concrete functions functions printing coq syntax.
-----------------------------------------------------------------------------*)
(* List of imports *)

type import = string list
type scope = string list

let import (import : import) =
  cmd_stm Require ~keyword_list:[Import] (String.concat " " import)

let scope (scope : scope) =
  cmd_stm Open ~keyword_list:[Scope] (String.concat " " scope)

(* Sorts *)

type sort_dec = sort list

(* In the script, constructors names as preappended with a C. *)
let sort_to_cnstr (s : sort) =
  String.concat "" ["C"; sort_to_string s]

let sort_def_stm (sort_dec : sort_dec) =
  let def_body =
    ident_vbar String.empty (List.map (fun s -> (sort_to_cnstr s, String.empty, String.empty)) sort_dec)
  in
  cmd_def Definition "base_types" def_body

let rec sort_abrv (sort_list : sort_dec) =
  match sort_list with
  | [] -> String.empty
  | hd :: tl ->
    let def_str =
      cmd_def Definition (sort_to_string hd) (sort_to_cnstr hd)
    in def_str ^ "\n" ^ sort_abrv tl

(* Function symbols *)

type fn_dec = fn list

let fn_to_ctrs f =
  "T" ^ (fn_to_string f)

let fn_def_stm fn_dec =
  let def_body =
  ident_vbar String.empty (
    List.map (fun s -> (fn_to_ctrs s, "", "")) fn_dec
  )
  in
  cmd_def Definition "fun_symbols" def_body

let arity_def_stm fn_list =
  let match_body =
    ident_vbar "  " (
      List.map (fun s ->
        (fn_to_ctrs s, "=>", ty_to_string (arity s))) fn_list
    )
  in
  let def_body =
    match_cmd "f" match_body
  in cmd_def Definition "map_ar f" def_body

let rec fn_abrv = function
  | [] -> String.empty
  | hd :: tl ->
    let (fn_name, args) = (fn_to_string hd, Syntax.Ty.make_args_names (arity hd))
    in(
      let args_str = String.concat " " args in
      let app_args =
        if args == [] then "" else "· " ^ (String.concat " · " args)
      in
      (cmd_def Definition
      (String.concat " " [fn_name;"{C}";":";args_str;"tm map_ar C _"])
      (String.concat " " ["BaseTm";(fn_to_ctrs hd);app_args]))
      ^ "\n" ^ fn_abrv tl
    )

(* Constant Proofs, always added to the script. *)
let dec_eq_ty_proof =
  cmd_proof Defined "decEq_finite."

let dec_eq_fn_proof =
  cmd_proof Defined "decEq_finite."

let dec_eq_ty =
  (cmd_stm Global ~keyword_list:[Instance] "decEq_base_types : decEq base_types") ^ "\n" ^
  dec_eq_ty_proof

let dec_eq_fn =
  cmd_stm Global ~keyword_list:[Instance] "decEq_fun_symbols : decEq fun_symbols" ^ "\n" ^
  dec_eq_fn_proof
