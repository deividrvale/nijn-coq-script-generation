(* open Syntax.Ty.SType *)


type answer = YES | NO | MAYBE

let answer_to_string = function
  | YES -> "YES"
  | NO -> "NO"
  | MAYBE -> "MAYBE"

(*-----------------------------------------------------------------------------
  Type Declaration
-----------------------------------------------------------------------------*)
(*
  Type declarations are not types, but they can be used to generate types.
  The from wanda's output
  <fun_name : string> : [fakety * ... * fakety ] --> <sort_name : string >
*)

module StringSet = Set.Make(String)

type fakeTy =
  | Name of string
  | Arr of fakeTy * fakeTy

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

type dom =
  | In of fakeTy list
and im =
  | Out of string
and tydec =
  | Dec of dom * im

let dom_equal x y =
  match (x, y) with
  | (In ty1, In ty2) -> List.equal fake_ty_equal ty1 ty2

let im_equal x y =
  match (x, y) with
  | (Out st1, Out st2) -> String.equal st1 st2

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

(* Unique names in a type declaration (as I'm using sets.) *)
let names_of_tydec = function
  Dec (In i, Out o) ->
      StringSet.union (
        List.fold_left
        (StringSet.union)
        StringSet.empty
        (List.map names_of_fakeTy i)
      ) (names_of_fakeTy (Name o))

type sign = (Syntax.Term.fn * tydec) list
