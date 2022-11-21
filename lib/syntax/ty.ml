(*-----------------------------------------------------------------------------
  Types
-----------------------------------------------------------------------------*)
module type TYPE = sig
  type 'a t =
    | Base of 'a
    | Arrow of 'a t * 'a t
end

module MakeSType = functor (B : Symb.NAME) -> struct
  (* A sort type is a name from B *)
  type sort = B.t

  type ty =
    | Base of sort
    | Arrow of ty * ty

  (* Encapsulates functions dealing with sorts *)
  let sort_equal = B.equal

  let sort_list = B.symb_list

  let get_sort = B.get_symb_opt

  let sort_register = B.register_name

  let sort_to_string = B.to_string

  (* Simple Types *)
  let base_ty_mk b = Base b

  let arr_ty_mk a b = Arrow (a, b)


  let rec ty_equal ty1 ty2 =
    match (ty1, ty2) with
    | (Base _, Arrow _) -> false
    | (Arrow _, Base _) -> false
    | (Base x, Base y)  -> sort_equal x y
    | (Arrow (a,b), Arrow (c,d)) -> (ty_equal a c) && (ty_equal b d)

  let rec ty_order ty =
    match ty with
    | Base _ -> 0
    | Arrow (a,b) -> Int.max ( ty_order a + 1) (ty_order b)

  let is_base = (function
    | Base _ -> true
    | Arrow _ -> false)

  let rec ty_to_string ty =
    match ty with
    | Base b -> sort_to_string b
    | Arrow (a,b) ->
      if (is_base a) then
        (ty_to_string a) ^ " ⟶ " ^ (ty_to_string b)
      else
        "(" ^ (ty_to_string a) ^ ")" ^ " ⟶ " ^ (ty_to_string b)
end

(* Sorts are indexed names *)
module Sort = Symb.IndexedName()
module SType = MakeSType(Sort)
open SType

let rec make_args_names' ty n m =
  match ty with
  | Base _ -> []
  | Arrow (a, b) ->
    if is_base a then
      ["x"^(Int.to_string n)] @ make_args_names' b (n + 1) m
    else
      ["F"^(Int.to_string m)] @ make_args_names' b n (m + 1)

let make_args_names ty =
  make_args_names' ty 0 0
