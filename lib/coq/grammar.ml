(*-----------------------------------------------------------------------------
  The type of keywords
-----------------------------------------------------------------------------*)

type keyword =
  | Require | Import   | Open    | Scope
  | Proof   | Qed      | Defined | Definition
  | Match   | With     | End     | Let | In
  | Global  | Instance | Progam  | Inductive

let keyword_to_string = function
  | Require -> "Require"    | Import -> "Import"
  | Open -> "Open"          | Scope -> "Scope"
  | Proof -> "Proof"        | Qed -> "Qed"
  | Defined -> "Defined"    | Definition -> "Definition"
  | Match -> "match"        | With -> "with"
  | End -> "end"            | Let -> "let"
  | In -> "in"              | Global -> "Global"
  | Instance -> "Instance"  | Progam -> "Program"
  | Inductive -> "Inductive"

(*-----------------------------------------------------------------------------
  Generic functions printing coq syntax
-----------------------------------------------------------------------------*)
(* The functions below output strings of pieces of Coq syntax.
The idea is that proof script functions compose the constructors
here in order to print a coq proof.
*)

(*
* Vertical bars with identation.
The argument idt is intended to be used as a string with only spaces,
which defines the identation of each item in the vertical bar list.
Given a triple (lhs, token, rhs) and idt, it should return:

<idt>| <lhs> <token> <rhs>

The function below generalize this idea for a triple of strings.
*)
let rec vbar idt (ls : (string * string * string) list) =
  let open String in
  match ls with
  | [] -> empty
  | (lhs, token, rhs) :: [] ->
    concat empty [idt; "| "; lhs; token; rhs]
  | (lhs, token, rhs) :: tl ->
    let vbared =
      (concat empty [idt; "| "; lhs; " "; token; " "; rhs; "\n"] )
    in concat empty [vbared; vbar idt tl]


(* generate the following coq construct
    <keyword> <ident_dec> :=
    <def_body>
    .
*)
let cmd_def keyword ident_dec def_body =
  let open String in
  concat " " [
    keyword_to_string keyword;
    ident_dec;
    ":=";
    "\n"
  ] ^ def_body ^ "."

let cmd_stm ?keyword_list keyword stm_body =
  let open String in
  let stm =
  (fun extra_kw ->
    let header =
      concat " " [keyword_to_string keyword; extra_kw] ^ " " in
    header ^ stm_body ^ "."
  ) in
  match keyword_list with
  | None -> stm empty
  | Some keywords ->
    concat empty (List.map keyword_to_string keywords)
    |> stm

(* Generate the following:
Proof.
<proof_body>
<qed>.

The <qed> argument, which is of type keyword, is used to stablish how
the proof should end.
It is usually, Qed or Defined.
*)

let cmd_proof qed proof_body =
  keyword_to_string Proof ^ "." ^ "\n" ^
  proof_body ^ "\n" ^
  keyword_to_string qed ^ "."

let cmd_ind_dec keyword ident_dec obj_list =
  let def_body = vbar "" obj_list in
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
