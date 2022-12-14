open File.Wanda
open Parser

module LexPar : PARSER with type t = Par.token = struct
  type t = Par.token
  include Lex
  include Par
end

module P = ParseWithErrors (LexPar)
include P

let wanda_lexer : lexer = Lex.lexer

let p_answer : answer parser = Par.answer

let p_debug : 'a parser = Par.debug_parser

let p_file : 'a parser = Par.file
