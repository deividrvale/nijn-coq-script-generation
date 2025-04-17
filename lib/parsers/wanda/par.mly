%{
  open File.Wanda
  open Syntax.Poly
%}

// Tokens
%token YES
%token NO
%token MAYBE

%token <string> STRING
%token <int> INT
%token RW_ARR
%token TY_ARR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRAKT
%token RBRAKT
%token SIG_ID
%token RMD_ID
%token INT_ID
%token RULE_ID
%token POLY_ID
%token RREM_ID
%token CERT_ID
%token COLON
%token COMMA
%token TLAM
%token DOT
%token EQ
%token PLAM

%token PLUS
%token STAR

%token SEP
%token EOF

// Start symbols
%start answer signature trs interpretation file debug_parser

// Associativity and precedence level for the tokens.
//Important: the precedences must be listed from low to high.

%right TY_ARR
%right PLUS
%left STAR

// Types of each start declaration
%type < answer > answer
%type < string * fakeTy > fn_dec
%type < signature > signature
%type < (term_tree * term_tree) list > trs

%type < (string * poly_fun) list > interpretation

%type < (string * poly_fun) list > int_data
%type < (int list * ((string * poly_fun) list)) list> rrem_data



// debug parser, the type is abstract
%type < 'a > debug_parser
%type < parsed_file > file

%%

// Parametrized syntax definitions --------------------------------------------

(** Non-empty pairs. *)
%inline pair(X,Y):
  | LPAREN fst = X COMMA snd = Y RPAREN { (fst, snd) }

%inline neList(XS):
  | xs = delimited(LBRACE, separated_nonempty_list(SEP, XS), RBRACE) { xs }

// Syntax for file
answer:
    | YES   { YES }
    | NO    { NO }
    | MAYBE { MAYBE }

baseT:
    | STRING { $1 }
    | LPAREN baseT RPAREN { $2 }

fake_ty:
    | baseT { Name $1 }
    | fake_ty TY_ARR fake_ty { Arr ($1, $3) }
    | LPAREN fake_ty RPAREN { $2 }

fn_dec:
    | STRING COLON fake_ty { ($1, $3) }

signature:
    | SIG_ID COLON arity = neList(fn_dec) { arity }

// Terms
term_tree:
  | non_app { $1 }
  | app     { $1 }
  | TLAM STRING DOT term_tree { Lam($2, $4) }

app:
  | app non_app { App ($1,$2) }
  | non_app non_app { App($1,$2) }

non_app:
  | symb                    { $1 }
  | LPAREN term_tree RPAREN { $2 }

symb:
  | STRING { S $1 }

// Rewrite Rules
rule:
  | term_tree RW_ARR term_tree { ($1, $3) }

trs:
  | RULE_ID COLON rs = neList(rule) { rs }

// Polynomials
poly:
  | non_poly_app        { $1 }
  | poly_app            { $1 }

poly_app:
  | poly_app non_poly_app { app $1 $2 }
  | non_poly_app non_poly_app { app $1 $2}

non_poly_app:
  | INT    { num $1 }
  | STRING { var (PolV.register_name $1) }
  | LPAREN poly RPAREN { $2 }
  | poly PLUS poly  { add $1 $3 }
  | poly STAR poly { mul $1 $3 }
  (* F(x1, x2, ..., xn) *)
  | STRING LPAREN args = separated_nonempty_list(COMMA, poly) RPAREN
    { apply_poly_list (var (PolV.register_name $1)) args }

fun_poly:
  | PLAM xs = neList(STRING) DOT p = poly
    {
      let names = List.map (fun s -> PolV.register_name s ) xs in
      poly_fun_mk names p
    }

fn_int:
  | STRING LPAREN f = STRING RPAREN EQ p = fun_poly
    { l_interpret f p}
  | STRING LPAREN f = STRING RPAREN EQ p = poly
    { c_interpret f p }

int_data:
  | is = separated_nonempty_list(SEP, fn_int) { List.map proof_int is }

rrem_data:
  | RREM_ID { [([], [])] }

interpretation:
  | INT_ID COLON LBRACE itp = int_data RBRACE { itp }

cert_type:
  | POLY_ID { POLY }
  | RREM_ID { RREM }

// certificate:
//   | CERT_ID LPAREN  RPAREN EQ LBRAKT data =  RBRAKT { data }

(** represents the parser for files *)
file:
  | answer signature trs interpretation EOF { new_file $1 $2 $3 $4 }

debug_parser:
    | rrem_data EOF { $1 }
