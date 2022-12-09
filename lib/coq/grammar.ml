(*-----------------------------------------------------------------------------
  Generic functions printing coq syntax.
-----------------------------------------------------------------------------*)
type keyword =
  | Require | Import | Open    | Scope
  | Proof   | Qed    | Defined | Definition
  | Match   | With   | End | Let | In
  | Global  | Instance | Progam | Inductive

let keyword_to_string = function
  | Require -> "Require" | Import -> "Import"
  | Open -> "Open"       | Scope -> "Scope"
  | Proof -> "Proof"     | Qed -> "Qed"
  | Defined -> "Defined" | Definition -> "Definition"
  | Match -> "match"     | With -> "with"
  | End -> "end"         | Let -> "let"
  | In -> "in"           | Global -> "Global"
  | Instance -> "Instance" | Progam -> "Program"
  | Inductive -> "Inductive"

(* Vertical bars:
    idt is the identation character
    a list of triples: (<lhs, token, rhs>)
    wich prints a line as | T => B.
*)
let rec ident_vbar idt (ls : (string * string * string) list) =
  match ls with
  | [] -> String.empty
  | (lhs, token, rhs) :: [] ->
    String.concat "" [idt; "| "; lhs; token; rhs]
  | (lhs, token, rhs) :: tl ->
    let n = (
      String.concat
      ""
      [idt; "| "; lhs; " "; token; " "; rhs; "\n"]) in
      String.concat "" [n; ident_vbar idt tl]

(* generate the following coq construct
    <keyword> <ident_dec> :=
    <def_body>
    .
*)
let cmd_def keyword ident_dec def_body =
  String.concat " " [
    keyword_to_string keyword;
    ident_dec;
    ":=";
    "\n"
  ] ^
  def_body ^ "."

let cmd_stm ?keyword_list keyword stm_body =
  let stm = (fun extra_kw ->
    let header =
      String.concat " " [keyword_to_string keyword; extra_kw] ^ " " in
    header ^
    stm_body ^ "."
  ) in
  match keyword_list with
  | None -> stm ""
  | Some keywords ->
    String.concat "" (List.map keyword_to_string keywords)
    |> stm

let cmd_proof qed proof_body =
  keyword_to_string Proof ^ "." ^ "\n" ^
  proof_body ^ "\n" ^
  keyword_to_string qed ^ "."

let cmd_ind_dec keyword ident_dec obj_list =
  let def_body = ident_vbar "" obj_list in
  cmd_def keyword ident_dec def_body

let match_cmd key body =
  let m_cmd = keyword_to_string Match in
  let w_cmd = keyword_to_string With in
  (String.concat " " [m_cmd; key; w_cmd]) ^
  "\n" ^ body ^ "\n" ^
  (keyword_to_string End)

let let_cmd key body =
  String.concat " " [
    keyword_to_string Let;
    key;
    ":=";
    body;
    keyword_to_string In;
    "\n"
  ]
