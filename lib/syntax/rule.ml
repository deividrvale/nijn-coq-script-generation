open Term

exception Invalid_rule of string

type rule = term * term

type trs = rule list

(* TODO: Implement rule conditions *)
let rule_mk lhs rhs =
  (lhs, rhs)

let lhs = fst

let rhs = snd

let equal (r : rule) (r' : rule) : bool =
  match (r, r') with
  | ((lhs, rhs), (lhs', rhs')) ->
    (term_equal lhs lhs') && (term_equal rhs rhs')

let to_string r =
  tm_to_string (lhs r) ^ "==>" ^ tm_to_string (rhs r)

let get_label rule trs : string =
  "rule_" ^
  Int.to_string (Utils.Lists.index_of equal rule trs)
