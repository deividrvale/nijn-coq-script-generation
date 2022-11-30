%{
  open Syntax.Term
  open File.Wanda
%}

// Tokens
%token YES
%token NO
%token MAYBE

%token <string> STRING
%token RW_ARR
%token TY_ARR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SIG_ID
%token VAR_ID
%token RULE_ID
%token HAS_TYPE

%token SEP
%token EOF

// Start symbol
%start answer

// Types of each declaration
%type < answer > answer

%%

answer:
    | YES   { YES }
    | NO    { NO }
    | MAYBE { MAYBE }

