open Utils
module PolV = Symb.IndexedName ()
type name = PolV.t

(* Untyped polynomials *)
type poly =
  | Num of int
  | Var of name
  | Add of poly * poly
  | Mul of poly * poly
  | App of poly * poly

let num = fun x -> Num x
let var = fun x -> Var x
let add = fun x y -> Add(x,y)
let mul = fun x y -> Mul(x,y)
let app = fun x y -> App(x,y)

let rec equal p q =
  match (p,q) with
  | (Num i, Num j) -> Int.equal i j
  | (Var x, Var y) -> PolV.equal x y
  | (Add (p,q), Add (p',q')) ->
    equal p p' && equal q q'
  | (Mul (p,q), Mul (p',q')) ->
    equal p p' && equal q q'
  | (App (p, q), App (p', q')) ->
    equal p p' && equal q q'
  | _ -> false

type poly_fun =
  Polfun of name list * poly

let poly_fun_mk = fun xs p -> Polfun (xs, p)

let get_names (Polfun (xs, _)) = xs

let get_poly (Polfun (_, p)) = p

let rec get_poly_vars' poly vs =
  match poly with
  | Num _ -> []
  | Var v ->
    Lists.cons_uniq PolV.equal v vs
  | Add (pol1, pol2) ->
    (get_poly_vars' pol1 vs) @ (get_poly_vars' pol2 vs)
  | Mul (pol1, pol2) ->
    (get_poly_vars' pol1 vs) @ (get_poly_vars' pol2 vs)
  | App (p, q) -> get_poly_vars' p vs @ get_poly_vars' q vs

let get_vars poly =
  Lists.remove_duplicates
    PolV.equal (get_poly_vars' poly [])

(* test whether a variable x occurs in the poly p*)
let rec var_occurs (poly : poly) (x : name) =
  let or_test = (fun p q ->
    (var_occurs p x) || (var_occurs q x)
  ) in
  match poly with
  | Num _ -> false
  | Var v -> PolV.equal v x
  | Add (p, q) -> or_test p q
  | Mul (p, q) -> or_test p q
  | App (p, q) -> or_test p q

let rec reduce = function
  (* Unit for Add *)
  | Add (Num 0, p) -> p
  | Add (p, Num 0) -> p
  (* Unit for Mult *)
  | Mul (Num 1, p) -> p
  | Mul (p, Num 1) -> p
  (* Zero element *)
  | Mul (Num 0, _) -> Num 0
  | Mul (_, Num 0) -> Num 0
  (* Addition of Nums *)
  | Add (Num i, Num j) -> Num (i + j)
  | Mul (Num i, Num j) -> Num (i * j)
  (* Right-associativy for Add *)
  | Add (Add (p,q), l) -> Add (p, Add (q, l))
  (* Left-Associativity for Mul *)
  | Mul (p, Mul (q,l)) -> Mul (Mul (p,q), l)
  (* Distribution laws *)
  | Mul (p, Add (q,l)) -> Add (Mul (p,q), Mul (p, l))
  | Mul (Add (q,l), p) -> Add (Mul (p,q), Mul (p, l))
  (* Compatibility Rules *)
  | App (v, p) -> App (v, reduce p)
  | Mul (p, q) -> Mul (reduce p, reduce q)
  | Add (p, q) -> Add (reduce p, reduce q)
  | p -> p

(* Apply f to itself until a fixpoint is reached *)
let rec fix_point (eq : 'a -> 'a -> bool) (f : 'a -> 'a) (x : 'a) =
  let f_a = f x in
    if eq f_a x then x else fix_point eq f f_a

let simplify = fix_point equal reduce

let rec to_string p =
  let poly = simplify p in
  match poly with
  | Num n -> "P_const " ^ (Int.to_string n)
  | Var v -> PolV.to_string v
  | Add (p, q) ->
    to_string p ^ " + " ^ to_string q
  | Mul (p, q) ->
    to_string p ^ " * " ^ to_string q
  | App (p, q) ->
    "(" ^
      to_string p ^ " ·P " ^ "(" ^ to_string q ^ ")" ^
    ")"

let apply_poly_list poly poly_list =
  List.fold_left (fun x y -> App(x,y)) poly poly_list
