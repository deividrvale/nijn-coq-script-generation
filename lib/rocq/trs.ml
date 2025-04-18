open Grammar
open Syntax.Ty.SType
open Syntax.Term
open Syntax.Poly

(*-----------------------------------------------------------------------------
  Concrete functions functions printing coq syntax.
-----------------------------------------------------------------------------*)

(* List of imports ----------------------------------------------------------*)

type import = string list
type scope  = string list

let import (import : import) =
  cmd_stm Require ~keyword_list:[Import] (String.concat " " import)

let scope (scope : scope) =
  cmd_stm Open ~keyword_list:[Scope] (String.concat " " scope)

(* Sorts --------------------------------------------------------------------*)

type sort_dec = sort list

(* In the script, constructors names as preappended with a C. *)
let sort_to_cnstr (s : sort) =
  String.concat "" ["C"; sort_to_string s]

let sort_def_stm (sort_dec : sort_dec) =
  let open String in
  let ss =
    List.map (fun s -> (sort_to_cnstr s, empty, empty)) sort_dec in
  let def_body = vbar empty ss in
  cmd_def Inductive "base_types" def_body

let rec sort_abrv (sort_list : sort_dec) =
  match sort_list with
  | [] -> String.empty
  | hd :: tl ->
    let def_str =
      cmd_def Definition (sort_to_string hd) ("Base " ^ (sort_to_cnstr hd))
    in def_str ^ "\n" ^ sort_abrv tl

(* Fun Symbols --------------------------------------------------------------*)

type fn_dec = fn list

let fn_to_ctrs f =
  "T" ^ (fn_to_string f)

let fn_def_stm fn_dec =
  let def_body =
    vbar String.empty (List.map (fun s -> (fn_to_ctrs s, "", "")) fn_dec) in
  cmd_def Inductive "fun_symbols" def_body

let arity_def_stm fn_list =
  let match_body =
    vbar "  " (
      List.map (fun s ->
        (fn_to_ctrs s, " => ", ty_to_string (arity s))) fn_list
    )
  in
  let def_body =
    match_cmd "fn_symbols" match_body
  in cmd_def Definition "fn_arity fn_symbols" def_body

let rec fn_abrv = function
  | [] -> String.empty
  | hd :: tl ->
    let fn_name = fn_to_string hd in
      (cmd_def Definition
      (String.concat " " [fn_name;"{C}";":";"tm fn_arity C _"])
      (String.concat " " ["BaseTm";(fn_to_ctrs hd)]))
      ^ "\n" ^ fn_abrv tl

(* Rules --------------------------------------------------------------------*)
(* Helper function: generate a coq rule context of size n. *)
let gen_ctx n =
  let ctx_dash = List.init n (fun _ -> "_") in
  "(" ^ (String.concat " ,, " (ctx_dash @ ["∙"])) ^
  ")" ^ " _" ^ "\n"

(*  *)
let rules_def_stm (afs : Syntax.Rule.trs) =
  let open Syntax.Rule in
  let rec rules_def_stm' = (fun afs i ->
    match afs with
    | [] -> String.empty
    | hd :: tl ->
      (* The naming context for the rule is the free variables of the lhs. *)
      let fvars_ctx = (free_var (lhs hd)) in
      (* We then determine the size of such context, in order to generate the
         coq implicit context of correct size. *)
      let n = List.length fvars_ctx in
      (* generate the string for context *)
      let ctx = gen_ctx n in
      let def_body =
        (* index_of rule *)
        "rule_" ^ Int.to_string i ^
        " := " ^ "\n" ^
        (* name of the rule *)
        "    make_rewrite" ^ "\n" ^
        (* context *)
        "    " ^ ctx ^
        (* lhs *)
        "    " ^
        (nameless_to_string (terms_to_bruijn_ctx fvars_ctx (lhs hd))) ^ "\n" ^
        (* rhs *)
        "    " ^
        (nameless_to_string (terms_to_bruijn_ctx fvars_ctx (rhs hd)))
      in
      (cmd_stm Progam ~keyword_list:[Definition] def_body) ^ "\n" ^
      (rules_def_stm' tl (i + 1))
  ) in rules_def_stm' afs 0

(* TRS ----------------------------------------------------------------------*)
let afs_df_stm (afs : Syntax.Rule.trs) (name : string) =
  let rec rules_label_list = (fun trs ->
    match trs with
    | [] -> "List.nil"
    | hd :: tl ->
      Syntax.Rule.get_label hd afs ^
      " :: " ^
      (rules_label_list tl)
  ) in
  cmd_def Definition name
  ("  make_afs\n" ^
  "    fn_arity \n" ^
  "    (" ^ rules_label_list afs ^ ")")

(* Decidable equality -----------------------------------------------------*)

let dec_eq_ty_proof =
  cmd_proof Defined "decEq_finite."

let dec_eq_fn_proof =
  cmd_proof Defined "decEq_finite."

let dec_eq_ty =
  (cmd_stm Global
          ~keyword_list:[Instance]
          "decEq_base_types : decEq base_types") ^
  "\n" ^
  dec_eq_ty_proof

let dec_eq_fn =
  (cmd_stm Global
          ~keyword_list:[Instance]
          "decEq_fun_symbols : decEq fun_symbols") ^
  "\n" ^
  dec_eq_fn_proof

let trs_to_coq (data : Certificate.cert_data) =
  let open Syntax.Term in
  let open Syntax.Ty.SType in
  String.concat "\n" [
    (* Imports and Scope *)
    import ["Nijn.Nijn"];
    scope  ["poly_scope"] ^ "\n";
    (* Sorts *)
    sort_def_stm (sort_list ()) ^ "\n";
    dec_eq_ty ^ "\n";
    sort_abrv (sort_list ()) ^ "\n";
    (* Function Symbols *)
    fn_def_stm (fn_list ());
    dec_eq_fn;
    arity_def_stm (fn_list ());
    fn_abrv (fn_list ()) ^ "\n";
    (* Rules and Rewriting *)
    rules_def_stm data.trs;
    afs_df_stm data.trs "trs" ^ "\n"
  ]
