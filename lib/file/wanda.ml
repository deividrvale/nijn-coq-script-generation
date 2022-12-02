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

(*  *)
let rec fakeTy_to_ty = function
  | Name n -> Ty.SType.base_ty_mk (Ty.SType.get_sort n)
  | Arr(a,b) -> Ty.SType.arr_ty_mk (fakeTy_to_ty a) (fakeTy_to_ty b)

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

(* Type Declarations --------------------------------------------------------*)
(*
  Type declarations are not types, but they can be used to generate types.
  The from wanda's output
  <fun_name : string> : [fakety * ... * fakety ] --> <sort_name : string >
*)
type dom =
  | In of fakeTy list
and im =
  | Out of fakeTy
and tydec =
  | Dec of dom * im

let rec dom_to_ty (In xs) =
  match xs with
  | [] -> []
  | hd :: tl ->
    (fakeTy_to_ty hd) :: (dom_to_ty (In tl))

let im_to_ty (Out ft) =
  fakeTy_to_ty ft

let dom_equal x y =
  match (x, y) with
  | (In ty1, In ty2) -> List.equal fake_ty_equal ty1 ty2

let im_equal x y =
  match (x, y) with
  | (Out st1, Out st2) -> fake_ty_equal st1 st2

let tydec_equal x y =
  match (x,y) with
  | (Dec(d1,i1), Dec(d2, i2)) ->
    (dom_equal d1 d2) && (im_equal i1 i2)

let typedec_mk dom im =
  Dec(dom, im)

let typedec_get_dom = function
  Dec(In d, _) -> d

let typedec_get_im = function
  Dec(_, Out st) -> st

let tydec_to_ty (s : tydec) =
  let dom = dom_to_ty (In (typedec_get_dom s)) in
    let rec ds_to_ty = (fun ds ->
    match ds with
    | hd :: [] -> Ty.SType.arr_ty_mk hd (fakeTy_to_ty (typedec_get_im s))
    | hd :: tl ->
      Ty.SType.arr_ty_mk hd (ds_to_ty tl)
    | [] -> fakeTy_to_ty (typedec_get_im s)
    ) in ds_to_ty dom

(* Unique names in a type declaration (as I'm using sets.) *)
let names_of_tydec = function
  Dec (In i, Out o) ->
      StringSet.union (
        List.fold_left
        (StringSet.union)
        StringSet.empty
        (List.map names_of_fakeTy i)
      ) (names_of_fakeTy o)

type signature = (string * tydec) list

(* We need to register *)
let register_signature (s : signature) =
  (* The set of names in the signature are the
  *)
  let sort_names =
    Utils.Lists.remove_duplicates String.equal (
      StringSet.elements (
        List.fold_left StringSet.union StringSet.empty
        (List.map (fun x -> names_of_tydec (snd x)) s))
    )
  in List.iter (fun x -> let _ = Ty.SType.sort_register x in () ) sort_names;
  (* Register function symbol names *)
  let fn_names =
    List.map fst s
  in List.iter (fun x -> let _ = Term.fn_register x in ()) fn_names;
  (* Register arities for each function symbol *)
  List.iter (fun (f, ty) -> Term.fn_register_ty (Term.get_fn f) (tydec_to_ty ty)) s

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
      List.fold_left (fun x y -> Term.App(x,y)) (Term.Fun f) ( List.map tt_to_term' ts)
  ) in tt_to_term' t

let rec term_tree_to_string = function
  | S f -> f
  | Lam (x, t) ->
    "λ" ^ x ^ "." ^ term_tree_to_string t
  | FApp (f, args) ->
    f ^ Utils.Lists.to_string term_tree_to_string args
  | App(s,t) ->
    "(" ^ (term_tree_to_string s) ^ " " ^ (term_tree_to_string t) ^ ")"

type trs = (term_tree * term_tree) list

let get_trs (afs : trs) : Rule.trs =
  List.map (fun (x,y) -> Rule.rule_mk (tt_to_term x) (tt_to_term y)) afs

type poly_tree =
  | Num of int
  | FOName of string
  | App of string * poly_tree
  | Add of poly_tree * poly_tree
  | Mul of poly_tree * poly_tree

(* let rec poly_tree_to_poly = function
  | Num i -> Poly.Num i
  | FOName x ->
    if Option.is_some (Poly.PolV.get_symb_opt x) then
      Poly.Var (Poly.FOVar (Poly.PolV.get_symb x))
    else
      let name = Poly.PolV.register_name x in
      Poly.Var (Poly.FOVar (name))
  | App (x, p) ->
    if Option.is_some (Poly.PolV.get_symb_opt x) then
      Poly.App (Poly.HOVar (Poly.PolV.get_symb x), poly_tree_to_poly p)
    else
      let name = Poly.PolV.register_name x in
      Poly.App (Poly.HOVar name, poly_tree_to_poly p)
  | Add (p, p') ->
    Poly.Add (poly_tree_to_poly p, poly_tree_to_poly p')
  | Mul (p,p') ->
    Poly.Mul (poly_tree_to_poly p, poly_tree_to_poly p') *)


type fun_poly =
  | FPoly of string list * poly_tree

(* let to_fn_poly = function *)
  (* | FPoly (ns, p) ->d *)
    (* refactor poly so there is no difference between fo and ho vars *)




type file = {
  ans : answer;
  sign : signature;
  afs : trs;
  rmd : trs
}

let new_file ans arity afs rmd = {
  ans = ans;
  sign = arity;
  afs = afs;
  rmd = rmd
}

let process_file file =
  (* first, we register the signature *)
  let _ = register_signature file.sign in
  get_trs file.afs
