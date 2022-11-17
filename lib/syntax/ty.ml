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

  let register_sort = B.register_name

  let to_string = B.to_string

  (* Simple Types *)
  let base_ty_mk name =
    let name_opt = get_sort name in
    Option.map (fun x -> Base x) name_opt

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

end

(* Sorts are indexed names *)
module Sort = Symb.IndexedName()
module SType = MakeSType(Sort)
open SType
