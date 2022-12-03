open File.Wanda
open Parsers

module LexPar : PARSER with type t = Par.token = struct
  type t = Par.token
  include Lex
  include Par
end

module P = ParseWithErrors (LexPar)

open LexPar
include P

let wanda_lexer : lexer = Lex.lexer

let p_answer : answer parser = Par.answer

let p_debug : 'a parser = Par.debug_parser

let p_file : 'a parser = Par.file
