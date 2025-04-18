open Certificate

(* Rule Removal ------------------------------------------------------------- *)

let rec list_cmd (to_string : 'a -> string) (l : 'a list) : string =
  match l with
  | [] -> "List.nil"
  | hd :: tl ->
    String.concat " "
    [to_string hd; "::"; list_cmd to_string tl]


let rr_cert_tactic selector poly_name =
  let slt = list_cmd Int.to_string selector in
  String.concat String.empty [
    "rule_removal_certificate "; "("; slt; ")"; "."; "\n";
    "{ "; "poly_certificate "; poly_name; "."; " }"
    ]

let gen_rr_poly_defs (rr_data : ('a * poly_int) list) =
  let module T = Poly in
  let rec aux data i = (
    match data with
    | [] -> String.empty
    | hd :: tl -> (
      T.itp_def_stm (snd hd) ("rr_poly_" ^ (Int.to_string i)) "trs" ^ "\n"
    ) ^ aux tl (i + 1)
  ) in
  aux rr_data 0

let gen_rr_tactics (rr_data : (int list * 'a) list) =
  let rec aux data i = (
    match data with
    | [] -> "empty_certificate."
    | hd :: tl -> (
      rr_cert_tactic (fst hd) ("rr_poly_" ^ Int.to_string i) ^ "\n"
    ) ^ aux tl (i + 1)
  ) in
  "certificate_SN. \n" ^ aux rr_data 0

let rr_thm_stm trs_name =
  let open Grammar in
  (* let proof = cmd_proof Qed proof_tactics in *)
  cmd_stm Theorem ("trs_isSN : isSN " ^ trs_name)

let rr_to_coq (data : cert_data) =
  let open Grammar in
  let certificate = data.cert in
  let preamble = Trs.trs_to_coq data in
  match certificate with
  | Rem cert -> (
    (* Generate all the polynomials that are used in each step of the rule removal proof beforehand. *)
    let pols = gen_rr_poly_defs cert in
    (* Theorem stablishing strong normalization *)
    let sn_thm = rr_thm_stm "trs" in
    let sn_proof = cmd_proof Qed (gen_rr_tactics cert) in
    String.concat "\n"
    [preamble; pols; sn_thm; sn_proof]
  )
  | _ -> raise (WrongData "wrong data")
