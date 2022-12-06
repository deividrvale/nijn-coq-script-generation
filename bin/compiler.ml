exception Invalid_config of string

type format =
  | NONE
  | WANDA

let format_to_string = function
  | NONE -> "None"
  | WANDA -> "Wanda"

let format_eq f f' =
  match (f,f') with
  | (NONE, NONE) -> true
  | (WANDA, WANDA) -> true
  | _ -> false

type termination_technique =
  | NONE
  | POLY

let tt_to_string = function
  | NONE -> "None"
  | POLY -> "Polynomial Interpretations, no rule removal."

let tt_eq f f' =
  match (f,f') with
  | (NONE, NONE) -> true
  | (POLY, POLY) -> true
  | _ -> false

type config = {
  mutable format : format;
  mutable tt : termination_technique
}

let config = {
  format = NONE;
  tt = NONE
}

let config_to_string cf =
  String.concat " " [
    "Input format:";
    format_to_string cf.format;
    tt_to_string cf.tt
  ]

let get_initial_config () = config

let set_compiler (new_config : config) =
  config.format <- new_config.format;
  config.tt <- new_config.tt

(*  *)
let int_data = File.Wanda.process_file

(* Compile the parser and file module to generate a proof string
*)
let compile_wanda_no_rr file =
  let parsed_file =
    Wanda_parser.parse_from_string
    Wanda_parser.p_file
    Wanda_parser.wanda_lexer file
  in
  let int_data = File.Wanda.process_file parsed_file
  in File.Wanda.gen_proof_string int_data

let compile str =
  if format_eq config.format WANDA && tt_eq config.tt POLY then
    compile_wanda_no_rr str
  else
    raise (Invalid_config (config_to_string config))
