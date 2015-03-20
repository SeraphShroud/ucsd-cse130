%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token <string> Id
%token TRUE FALSE

%token LET REC EQ IN        /* let (rec) ... = ... in ... */
%token FUN ARROW            /* fun ... -> ... */
%token IF THEN ELSE         /* if ... then ... else ... */

%token OR                   /* || */
%token AND                  /* && */
%token LT LE NE             /* < , <= , != */
%token PLUS MINUS MUL DIV   /* + , - , * , / */
%token LPAREN RPAREN        /* ( , ) */

%token LBRAC RBRAC          /* [ , ] */
%token SEMI COLONCOLON      /* ; , :: */

/* TODO: Implement hd and tl */
/* %token HEAD TAIL */            /* hd , lt */

%nonassoc LET FUN IF
%left OR
%left AND
%left EQ LT LE NE
%right COLONCOLON SEMI RBRAC
%left PLUS MINUS 
%left MUL DIV
%left APP

%start exp 
%type <Nano.expr> exp

%%

exp: 
    | LET Id EQ exp IN exp      { Let ($2, $4, $6) }
    | LET REC Id EQ exp IN exp  { Letrec ($3, $5, $7) }
    | FUN Id ARROW exp          { Fun ($2, $4) }
    | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }

    | orexp                   { $1 }

orexp: /* OR expression */
    | orexp OR andexp           { Bin ($1, Or, $3) }
    | andexp                    { $1 }

andexp: /* AND expression */
    | andexp AND compexp        { Bin ($1, And, $3) }
    | compexp                   { $1 }

compexp: /* Comparison expression */
    | compexp EQ lexp          { Bin ($1, Eq, $3) }
    | compexp NE lexp          { Bin ($1, Ne, $3) }

    | compexp LT lexp          { Bin ($1, Lt, $3) }
    | compexp LE lexp          { Bin ($1, Le, $3) }
    | lexp                      { $1 }

lexp: /* List expression */
    | aexp1 COLONCOLON lexp     { Bin ($1, Cons, $3) }
    | aexp1 SEMI lexp           { Bin ($1, Cons, $3) }
    | LBRAC lexp                { $2 }
    | lexp RBRAC                { Bin ($1, Cons, NilExpr) }
    | aexp1                     { $1 }

aexp1: /* Low-priority arithmetic expression (+,-) */
    | aexp1 PLUS aexp2          { Bin ($1, Plus, $3) }
    | aexp1 MINUS aexp2         { Bin ($1, Minus, $3) }
    | aexp2                     { $1 }

aexp2: /* High-priority arithmetic expression (*,/) */
    | aexp2 DIV fexp            { Bin ($1, Div, $3) }
    | aexp2 MUL fexp            { Bin ($1, Mul, $3) }

    | fexp                      { $1 }

fexp: /* Function expression */
    | fexp bexp                 { App ($1, $2)}

    | bexp                      { $1 }

bexp: /* Base expression */
    | Num                       { Const $1 }
    | Id                        { Var $1 }
    | TRUE                      { True }
    | FALSE                     { False }

    | LPAREN exp RPAREN         { $2 }

    | LBRAC RBRAC               { NilExpr }