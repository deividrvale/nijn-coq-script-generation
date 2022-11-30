{
    open Lexing
    open Par

    exception SyntaxError of string
}

(* Definitions for textual structure *)
let espace  = [' ' '\t']+
let name    = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let newLine = '\r' | '\n' | "\r\n"

(*  *)
rule lexer = parse
  | newLine         { new_line lexbuf; lexer lexbuf }
  | espace          { lexer lexbuf }
  | "(*"            { commentaire lexer lexbuf }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "["             { LBRACE }
  | "]"             { RBRACE }
  | ":"             { HAS_TYPE  }
  | ";"             { SEP }
  | "YES"           { YES }
  | "NO"            { NO }
  | "MAYBE"         { MAYBE }
  | "signature"     { SIG_ID }
  | "vars"          { VAR_ID }
  | "rules"         { RULE_ID }
  | "==>"              { RW_ARR }
  | "-->"              { TY_ARR }
  | name               { STRING (Lexing.lexeme lexbuf) }
  | _                  { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}
  | eof             { EOF }

and commentaire continuation = parse
  | "*)" { continuation lexbuf }
  | "(*" { commentaire (commentaire continuation) lexbuf }
  | _    { commentaire continuation lexbuf }
