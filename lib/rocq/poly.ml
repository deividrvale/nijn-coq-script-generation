open Grammar
open Syntax.Ty.SType
open Syntax.Term
open Syntax.Poly
open Certificate
module TRS = Trs

(* Interpretation -----------------------------------------------------------*)

(* Helper function:
Integer indexes to Coq context indexes notation *)
let rec to_ctx_idx i =
  if i <= 0 then "Vz" else "(Vs " ^ to_ctx_idx (i - 1) ^ ")"

(* Helper function:
Print the formal statement of a poly variable.
Given that polynomial interpretations are of the form
Lam [x0, ..., xn] . <Poly>,
the printing of each of of the x_i's depends whether
the x_i's appear in the body of <Poly>.
If it does, we declare it with a name, otherwise
we just print a Lam P.*)
let poly_var_to_stm (v : name) occurs v_idx =
  if occurs then
  begin
    "λP let " ^
    Syntax.Poly.PolV.to_string v ^
    " := " ^
    "P_var " ^
    to_ctx_idx v_idx ^
    " in"
  end
  else
    "λP"

(* Helper function:
    Print the formal declaration of each variable in
    the polynomial function Lam [x0, ..., xn] . <poly> *)
let poly_vars_to_stm ( f : poly_fun) =
  let (vs, pol) = (get_names f, get_poly f) in
  let rec poly_vars_to_stm' = (fun vs p idx ->
    match vs with
    | [] -> ""
    | hd :: tl ->
      let occurs = var_occurs p hd in
      (poly_var_to_stm hd occurs idx) ^ "\n" ^
      poly_vars_to_stm' tl p (idx - 1)
  ) in poly_vars_to_stm' vs pol (List.length vs - 1)

(* Helper function:
    prints the polynomial function Lam [x0, ..., xn] . <poly>. *)
let poly_to_stm poly_fun =
  let pol = get_poly poly_fun in
  (poly_vars_to_stm poly_fun) ^
  "(" ^ "to_Poly (" ^ (to_string pol) ^ ")" ^ ")"

(*  *)
let poly_match_body (itp : (fn * poly_fun) list) =
    vbar ""
      (List.map (fun (f,p) -> (TRS.fn_to_ctrs f, " => \n", poly_to_stm p)) itp )

let itp_def_stm (itp : poly_int) def_name trs_name =
  let match_body = poly_match_body itp in
  let def_body =
  match_cmd "fn_symbols" match_body in
  cmd_def Definition
  (def_name ^ " fn_symbols : poly ∙ (arity " ^ trs_name ^ " fn_symbols)") def_body

let sn_def_stm (name : string) =
  let def_proof = cmd_proof Qed "solve_poly_SN map_fun_poly."
  in cmd_stm Definition ("trs_isSN : isSN " ^ name) ^ "\n" ^ def_proof

let poly_to_coq (data : cert_data) =
  let open Trs in
  let open Syntax.Term in
  let open Syntax.Ty.SType in
  (trs_to_coq data) ^
  String.concat "\n" [
    (* Interpretation *)
    (
      match data.cert with
      | Poly itp -> itp_def_stm itp "map_fun_poly" "trs" ^ "\n"
      | _ -> raise
        (WrongData "the function poly_to_coq should only
        get a certificate with polynomial interpretation")
    );
    sn_def_stm "trs"
  ]
