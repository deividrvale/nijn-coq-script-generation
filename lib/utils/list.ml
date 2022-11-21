(* Polymorphic utilities over lists. *)

(* index_of needs a comparison operator on the elements of the list *)
let rec index_of' (eq : ('a -> 'a -> bool)) (x : 'a) (xs : 'a list) (idx : int)
                  : int =
  match xs with
  | [] -> raise Not_found
  | hd :: tl ->
    if (eq x hd) then
      idx
    else
      index_of' eq x tl (idx + 1)

let index_of eq x xs =
  index_of' eq x xs 0

let rec remove (eq : 'a -> 'a -> bool) (x : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | hd :: tl ->
    if (eq x hd) then tl else remove eq x tl
