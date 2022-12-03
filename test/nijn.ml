open Syntax.Ty.SType
open Syntax.Term
open Syntax.Poly
open Coq.Proof_script
open Wanda_parser

let str =
"
YES
Signature: [

  append : [c * c] --> c ;
  cons : [b * c] --> c ;
  flatwith : [a -> b * b] --> c ;
  flatwithsub : [a -> b * c] --> c ;
  leaf : [a] --> b ;
  nil : [] --> c ;
  node : [c] --> b

]
Rules: [

  append(nil, x) => x ;
  append(cons(x, y), z) => cons(x, append(y, z)) ;
  flatwith(f, leaf(x)) => cons(f x, nil) ;
  flatwith(f, node(x)) => flatwithsub(f, x) ;
  flatwithsub(f, nil) => nil ;
  flatwithsub(f, cons(x, y)) => append(flatwith(f, x), flatwithsub(f, y))

]
Interpretation: [

  J(append) = Lam[y0;y1].y0 ;
  J(cons) = Lam[y0;y1].2 + 2*y0 ;
  J(flatwith) = Lam[G0;y1].2*y1 + 3*y1*G0(y1) ;
  J(flatwithsub) = Lam[G0;y1].3 + 3*y1 + G0(y1) + 3*y1*G0(y1) ;
  J(leaf) = Lam[y0].3 + 3*y0 ;
  J(nil) = 1 ;
  J(node) = Lam[y0].3 + 3*y0

]
Removed: [
  append(nil, X) => X ;
  append(cons(X, Y), Z) => cons(X, append(Y, Z)) ;
  flatwith(F, leaf(X)) => cons(F X, nil) ;
  flatwith(F, node(X)) => flatwithsub(F, X) ;
  flatwithsub(F, nil) => nil ;
  flatwithsub(F, cons(X, Y)) => append(flatwith(F, X), flatwithsub(F, Y))
]
"

let parsed_debug =
  Wanda_parser.parse_from_string Wanda_parser.p_debug
  Wanda_parser.wanda_lexer str

let int_data = File.Wanda.process_file parsed_debug

let () =
  print_endline (
    File.Wanda.gen_proof_string int_data
  )

(* Slowly Rebuilding the proof script from parsed data *)
 (* let () =
  (* Imports and Scope *)
  print_newline ();
  print_endline (import ["Nijn.Nijn"] );
  print_endline (scope ["poly_scope"]);
  print_newline ();
  (* Sorts *)
  print_endline (sort_def_stm (sort_list ()));
  print_newline ();
  print_endline dec_eq_ty;
  print_newline ();
  print_endline (sort_abrv (sort_list ()));
  print_newline ();
  (* Function Symbols *)
  print_endline (fn_def_stm (fn_list ()));
  print_newline ();
  print_endline dec_eq_fn;
  print_newline ();
  print_endline (arity_def_stm (fn_list ()));
  print_newline ();
  print_endline (fn_abrv (fn_list ()));
  print_endline (rules_def_stm int_data.trs);
  print_endline (afs_df_stm int_data.trs "trs");
  print_endline (itp_def_stm int_data.poly_int "trs");
  print_endline (sn_def_stm "trs") *)
