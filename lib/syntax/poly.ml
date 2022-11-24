open Utils
module PolV = Symb.IndexedName ()
type name = PolV.t

(* Poly Variables are split into two different categories *)
type fo_var =
  | FOVar of name
and ho_var =
  | HOVar of name

(* The type var is the union type for both variable types.
   It will be used for poly_fun, the functional representation of polies. *)
type var =
  | V1 of fo_var
  | V2 of ho_var

let var_equal v1 v2 =
  match (v1, v2) with
  | (V1 (FOVar n), V1 (FOVar m)) ->
    PolV.equal n m
  | (V2 (HOVar n), V2 (HOVar m)) ->
    PolV.equal n m
  | _ -> false

let var_to_string = function
  | V1 (FOVar n) -> PolV.to_string n
  | V2 (HOVar n) -> PolV.to_string n

(* Polynomial expressions, this captures exactly second-order polynomials. *)
type poly =
  | Num of int
  | Var of fo_var
  | Add of poly * poly
  | Mul of poly * poly
  | App of ho_var * poly

let rec poly_equal p q =
  match (p,q) with
  | (Num i, Num j) -> Int.equal i j
  | (Var (FOVar x), Var (FOVar y)) -> PolV.equal x y
  | (Add (p,q), Add (p',q')) ->
    poly_equal p p' && poly_equal q q'
  | (Mul (p,q), Mul (p',q')) ->
    poly_equal p p' && poly_equal q q'
  | (App (HOVar v, p), App (HOVar v', p')) ->
    PolV.equal v v' && poly_equal p p'
  | _ -> false


type poly_fun =
  Polfun of var list * poly

let rec get_poly_vars' poly vs =
  match poly with
  | Num _ -> []
  | Var ((FOVar _) as v)->
    Lists.cons_uniq var_equal (V1 v) vs
  | App (v, pol) ->
    Lists.cons_uniq var_equal (V2 v) (get_poly_vars' pol vs)
  | Add (pol1, pol2) ->
    (get_poly_vars' pol1 vs) @ (get_poly_vars' pol2 vs)
  | Mul (pol1, pol2) ->
    (get_poly_vars' pol1 vs) @ (get_poly_vars' pol2 vs)

let get_vars poly =
  Lists.remove_duplicates var_equal (get_poly_vars' poly [])

let rec to_string (poly : poly) (var_to_string : var -> string) =
  match poly with
  | Num n -> "P_const " ^ (Int.to_string n)
  | Var (FOVar _ as v) -> var_to_string (V1 v)
  | Add (pol1, pol2) ->
    to_string pol1 var_to_string ^ " + " ^ to_string pol2 var_to_string
  | Mul (pol1, pol2) ->
    to_string pol1 var_to_string ^ " * " ^ to_string pol2 var_to_string
  | App (v, pol) ->
    var_to_string (V2 v) ^
    " ·P " ^ "(" ^ to_string pol var_to_string ^ ")"

(* This section reduce *)

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

(* Converge applies a function *)
let rec fix_point (eq : 'a -> 'a -> bool) (f : 'a -> 'a) (x : 'a) =
  let f_a = f x in
    if eq f_a x then x else fix_point eq f f_a

let simplify = fix_point poly_equal reduce
