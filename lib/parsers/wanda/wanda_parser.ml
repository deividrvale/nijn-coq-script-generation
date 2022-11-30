open Parsers
open File.Wanda

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
