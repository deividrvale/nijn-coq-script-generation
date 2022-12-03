{
    open Lexing
    open Par

    exception SyntaxError of string
}

(* Definitions for textual structure *)
let espace  = [' ' '\t']+
let name    = ['~']? ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let digit   = ['0'-'9']
let number  = digit+
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
  | ":"             { COLON  }
  | "/\\"           { TLAM }
  | "."             { DOT }
  | ","             { COMMA }
  | ";"             { SEP }
  | "*"             { STAR }
  | "="             { EQ }
  | "Lam"           { PLAM }
  | "YES"           { YES }
  | "NO"            { NO }
  | "MAYBE"         { MAYBE }
  | "+"             { PLUS }
  | "Signature"     { SIG_ID }
  | "Rules"         { RULE_ID }
  | "Removed"       { RMD_ID }
  | "Interpretation" { INT_ID }
  | "=>"              { RW_ARR }
  | "->"              { TY_ARR }
  | "-->"           { DC_ARR }
  | name               { STRING (Lexing.lexeme lexbuf) }
  | number              { INT (int_of_string( Lexing.lexeme lexbuf )) }
  | _                  { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}
  | eof             { EOF }

and commentaire continuation = parse
  | "*)" { continuation lexbuf }
  | "(*" { commentaire (commentaire continuation) lexbuf }
  | _    { commentaire continuation lexbuf }
