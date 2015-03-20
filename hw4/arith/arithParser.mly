%{
open Arith
%}
%token <int> CONST
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token EOF
%start expr
%type <Arith.arith_expr> expr
%%
expr:
  | expr PLUS expr2    { Plus ($1, $3) }
  | expr MINUS expr2   { Minus ($1, $3) }
  | expr2              { $1 }

expr2:
  | expr2 TIMES expr3  { Times ($1, $3) }
  | expr2 DIVIDE expr3 { Divide ($1, $3) }
  | expr3              { $1 }

expr3:
  | CONST              { Const $1 }
  | VAR                { Var $1 }
  | LPAREN expr RPAREN { $2 }
