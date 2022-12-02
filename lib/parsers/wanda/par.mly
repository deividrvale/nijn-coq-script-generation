%{
  open File.Wanda
%}

// Tokens
%token YES
%token NO
%token MAYBE

%token <string> STRING
%token <int> INT
%token RW_ARR
%token TY_ARR
%token DC_ARR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SIG_ID
%token RMD_ID
%token COLON
%token COMMA
%token RULE_ID
%token TLAM
%token DOT
%token PLAM

// %token J
%token PLUS
%token STAR

%token SEP
%token EOF

// Start symbol
%start answer debug_parser

// Associativity and precedence level for the tokens.
//Important: the precedences must be listed from low to high.

%right TY_ARR
%right PLUS
%left STAR

// Types of each declaration
%type < answer > answer
%type < tydec > ty_dec
%type < string * tydec > fn_dec
%type < signature > signature
// debug parser, the type is abstract
%type < 'a > debug_parser

%%

answer:
    | YES   { YES }
    | NO    { NO }
    | MAYBE { MAYBE }

// Signature of the file
baseT:
    | STRING { $1 }
    | LPAREN baseT RPAREN { $2 }

fake_ty:
    | baseT { Name $1 }
    | fake_ty TY_ARR fake_ty { Arr ($1, $3) }
    | LPAREN fake_ty RPAREN { $2 }

dom:
    LBRACE ds = separated_list(STAR, fake_ty) RBRACE {In ds}

ty_dec:
    | dom DC_ARR fake_ty { Dec ($1, Out $3) }

fn_dec:
    | STRING COLON ty_dec { ($1, $3) }

signature:
    | SIG_ID COLON LBRACE arity = separated_nonempty_list(SEP, fn_dec) RBRACE { arity }

// Terms
term_tree:
  | non_app { $1 }
  | app     { $1 }
  | TLAM STRING DOT term_tree { Lam($2, $4) }

app:
  | app non_app { App ($1,$2) }
  | non_app non_app { App($1,$2) }

non_app:
  | symb { $1 }
  | LPAREN term_tree RPAREN { $2 }
  | STRING LPAREN args = separated_nonempty_list(COMMA, term_tree) RPAREN { FApp ($1, args) }

symb:
  | STRING { S $1 }

rule:
  | term_tree RW_ARR term_tree { ($1, $3) }

trs:
  | RULE_ID COLON LBRACE rs = separated_nonempty_list(SEP, rule) RBRACE { rs }

removed:
  | RMD_ID COLON LBRACE rs = separated_list(SEP, rule) RBRACE { rs }

file:
  | answer signature trs removed { new_file $1 $2 $3 $4 }

poly:
  | INT     { Num $1 }
  | STRING  { FOName $1 }
  | poly PLUS poly  { Add ($1, $3) }
  | poly STAR poly { Mul ($1, $3) }
  | STRING LPAREN poly RPAREN { App ($1, $3) }

fun_poly:
  | PLAM LBRACE xs = separated_nonempty_list(SEP, STRING) RBRACE DOT p = poly { FPoly (xs, p) }

debug_parser:
    | fun_poly EOF { $1 }
