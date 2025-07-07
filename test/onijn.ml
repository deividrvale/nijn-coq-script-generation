open Certificate

let file = "
YES
Signature: [
  cons : b -> c -> c ;
  leaf : a -> b ;
  mapt : (a -> a) -> b -> b ;
  maptlist : (a -> a) -> c -> c ;
  nil : c ;
  node : c -> b
]

Rules: [
  mapt F (leaf X) => leaf (F X) ;
  mapt G (node Y) => node (maptlist G Y) ;
  maptlist H nil => nil ;
  maptlist I (cons Z U) => cons (mapt I Z) (maptlist I U)
]

Interpretation: [
  J(cons) = Lam[y0;y1].3 ;
  J(leaf) = Lam[y0].3 ;
  J(mapt) = Lam[G0;y1].2 + 2*y1 + 3*y1*G0(y1) ;
  J(maptlist) = Lam[G0;y1].2*y1 + 3*y1*G0(y1) + 3*G0(y1) ;
  J(nil) = 3 ;
  J(node) = Lam[y0].3
]
"

(* let parsed_file =
  Wanda_parser.parse_from_string
  Wanda_parser.p_file
  Wanda_parser.wanda_lexer file
let int_data = File.Wanda.process_file parsed_file

let () =
  Rocq.Rrem.rr_to_coq int_data |> print_endline *)

let () =
  let open Wanda_parser in
  (* let string = "
  Certificate(RREM) = {(
    [1;2;3],
    [
      J(cons) = Lam[y0;y1].3 ;
      J(leaf) = Lam[y0].3 ;
      J(mapt) = Lam[G0;y1].2 + 2*y1 + 3*y1*G0(y1) ;
      J(maptlist) = Lam[G0;y1].2*y1 + 3*y1*G0(y1) + 3*G0(y1) ;
      J(nil) = 3 ;
      J(node) = Lam[y0].3
    ]
  )}" *)
  let string =
"
YES
Signature: [
  cons : b -> c -> c ;
  leaf : a -> b ;
  mapt : (a -> a) -> b -> b ;
  maptlist : (a -> a) -> c -> c ;
  nil : c ;
  node : c -> b
]

Rules: [
  mapt F (leaf X) => leaf (F X) ;
  mapt G (node Y) => node (maptlist G Y) ;
  maptlist H nil => nil ;
  maptlist I (cons Z U) => cons (mapt I Z) (maptlist I U)
]

Certificate(RREM) = {
  (
    [0;1;2;3],
    [
      J(cons) = Lam[y0;y1].3 ;
      J(leaf) = Lam[y0].3 ;
      J(mapt) = Lam[G0;y1].2 + 2*y1 + 3*y1*G0(y1) ;
      J(maptlist) = Lam[G0;y1].2*y1 + 3*y1*G0(y1) + 3*G0(y1) ;
      J(nil) = 3 ;
      J(node) = Lam[y0].3
    ]
  )
}
"
in
let parsed = parse_from_string p_file wanda_lexer string in
let processed_file = File.Wanda.process_file parsed in
let t = Rocq.cert_to_rocq processed_file in
print_endline t
