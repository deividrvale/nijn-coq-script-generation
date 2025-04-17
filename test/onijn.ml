(* let () =
  print_endline
  "Testing will be added.
  Even though not a priority for this project due to the fact that any bug
  that Onijn may have would imply in the
  Coq proof script being not type checked by Coq." *)

open Coq
open Grammar

let str =
  cmd_def Definition "teste" "body"

let () =
  print_endline str

let () =
  let p_string = "Certificate(RREM) = {     }" in
  let open Wanda_parser in
  let test = parse_from_string Wanda_parser.p_debug Wanda_parser.wanda_lexer p_string in
  (* print_endline (File.Wanda.cert_opt_to_string test); *)
  ()
