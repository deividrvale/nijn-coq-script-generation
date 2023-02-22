open Syntax

exception OutOfBoundSym of string

(*-----------------------------------------------------------------------------
 Inner representation of wanda's output
-----------------------------------------------------------------------------*)

(* Answer type collects the possible answers returned by Wanda. *)
type answer = YES | NO | MAYBE

(* Type Declarations --------------------------------------------------------*)
let answer_to_string = function
  | YES   -> "YES"
  | NO    -> "NO"
  | MAYBE -> "MAYBE"

module StringSet = Set.Make(String)

(* Fake Types ---------------------------------------------------------------*)
type fakeTy =
  | Name of string
  | Arr of fakeTy * fakeTy

let rec fakeTy_to_string = function
  | Name s -> s
  | Arr (a, b) ->
    "(" ^fakeTy_to_string a ^ "->" ^ fakeTy_to_string b ^ ")"

let rec fake_ty_equal x y =
  match (x,y) with
  | (Name _, Arr _) -> false
  | (Arr _, Name _) -> false
  | (Name n, Name m) -> String.equal n m
  | (Arr(a1,b1), Arr(a2,b2)) -> (fake_ty_equal a1 a2) && (fake_ty_equal b1 b2)

(* Collects the name of a fakeTy in a set (so no repetitions) *)
let rec names_of_fakeTy = function
  | Name x -> StringSet.singleton x
  | Arr(fake1, fake2) ->
    StringSet.union (names_of_fakeTy fake1) (names_of_fakeTy fake2)

let rec fakeTy_to_ty = function
  | Name n -> Ty.SType.base_ty_mk (Ty.SType.get_sort n)
  | Arr(a,b) -> Ty.SType.arr_ty_mk (fakeTy_to_ty a) (fakeTy_to_ty b)

(* Signatures ---------------------------------------------------------------*)
type signature = (string * fakeTy) list

(* Register signature register the names in the types
and in the function symbols from the signature. *)
let register_signature (s : signature) =
  (* The set of names in the signature are the *)
  let sort_names =
    Utils.Lists.remove_duplicates String.equal (
      StringSet.elements (
        List.fold_left StringSet.union StringSet.empty
        (List.map (fun x -> names_of_fakeTy (snd x)) s))
    )
  in List.iter (fun x -> let _ = Ty.SType.sort_register x in () ) sort_names;
  (* Register function symbol names *)
  let fn_names =
    List.map fst s
  in List.iter (fun x -> let _ = Term.fn_register x in ()) fn_names;
  (* Register arities for each function symbol *)
  List.iter (fun (f, ty) -> Term.fn_register_ty (Term.get_fn f) (fakeTy_to_ty ty)) s

(* Terms --------------------------------------------------------------------*)

type term_tree =
  | S of string
  | Lam of string * term_tree
  | App of term_tree * term_tree
  | FApp of string * (term_tree list)

let tt_to_term (t : term_tree) =
  let rec tt_to_term' = (fun s ->
    match s with
    | S name ->
      (* We test if [name] is registered as fn or var. *)
      if Option.is_some (Term.get_fn_opt name) then
        Term.Fun (Term.get_fn name)
      else
        if (Option.is_some (Term.get_var_opt name)) then
          Term.Var (Term.get_var name)
        else
          Term.Var (Term.var_register name)
    | Lam (name, term) ->
      if Option.is_some (Term.get_var_opt name) then
        let v = Term.get_var name in
        Term.Lam (v, tt_to_term' term)
      else
        let v = Term.var_register name in
        Term.Lam (v, tt_to_term' term)
    | App (t,t') ->
      Term.App (tt_to_term' t, tt_to_term' t')
    | FApp (name, ts) ->
      let f = Term.get_fn name in
      List.fold_left
        (fun x y -> Term.App(x,y))
        (Term.Fun f)
        (List.map tt_to_term' ts)
  ) in tt_to_term' t

let rec term_tree_to_string = function
  | S f -> f
  | Lam (x, t) ->
    "λ" ^ x ^ "." ^ term_tree_to_string t
  | FApp (f, args) ->
    f ^ Utils.Lists.to_string term_tree_to_string args
  | App(s,t) ->
    "(" ^ (term_tree_to_string s) ^ " " ^ (term_tree_to_string t) ^ ")"

(* Term rewriting systes ----------------------------------------------------*)

type trs = (term_tree * term_tree) list

let get_trs (afs : trs) : Rule.trs =
  List.map (fun (x,y) -> Rule.rule_mk (tt_to_term x) (tt_to_term y)) afs

(* Polynomials --------------------------------------------------------------*)

type poly_int =
  | CInt of string * Poly.poly
  | LInt of string * Poly.poly_fun

let c_interpret = fun x y -> CInt (x,y)
let l_interpret = fun x y -> LInt (x,y)

let proof_int = function
  | CInt (f, p) ->
    (f, Poly.poly_fun_mk [] p)
  | LInt (f, p) -> (f, p)

(* File ---------------------------------------------------------------------*)

type parsed_file = {
  ans : answer;
  sign : signature;
  afs : trs;
  itp : (string * Poly.poly_fun) list;
  rmd : trs
}

let new_file ans arity afs itp rmd = {
  ans = ans;
  sign = arity;
  afs = afs;
  itp = itp;
  rmd = rmd
}

(*-----------------------------------------------------------------------------
 Compilation of the internal file representation to proof script
-----------------------------------------------------------------------------*)
(* When we process the parsed file, names for the syntax signature are registered
  in their respective states.
  So, the only data needed to compile the TRS to the coq script is the rules and
  the polynomial interpretation. *)
type int_data = {
  trs : Rule.trs;
  poly_int : (Term.fn * Poly.poly_fun) list
}

let process_file file =
  (* first, we register the signature *)
  let _ = register_signature file.sign in
  { trs = get_trs file.afs;
    poly_int =
    (List.map (fun (f,p) ->
      (let _ = Term.fn_register f in (); Term.get_fn f, p)) file.itp )
  }

let gen_proof_string (data : int_data) =
  let open Coq.Proof_script in
  let open Term in
  let open Ty.SType in
  String.concat "\n" [
    (* Imports and Scope *)
    import ["Nijn.Nijn"];
    scope ["poly_scope"];
    "\n";
    (* Sorts *)
    sort_def_stm (sort_list ());
    dec_eq_ty;
    "\n";
    sort_abrv (sort_list ());
    "\n";
    (* Function Symbols *)
    fn_def_stm (fn_list ());
    dec_eq_fn;
    "\n";
    arity_def_stm (fn_list ());
    fn_abrv (fn_list ());
    "\n";
    (* Rules and Rewriting *)
    rules_def_stm data.trs;
    afs_df_stm data.trs "trs";
    "\n";
    (* Interpretation and Strong Normalization *)
    itp_def_stm data.poly_int "trs";
    sn_def_stm "trs"
  ]
