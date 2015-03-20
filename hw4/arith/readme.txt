This starter kit has a lexer and parser
for basic arithmetic expressions with + - * /
int constants and string variables

To build it:
  $ make clean
  $ make

To test it:
  $ ocaml arithLexer.cmo arithParser.cmo arith.cmo main.cmo
  # let exp1 = Main.parse_string "2*3+5";;
  # Arith.eval_expr [] exp1;;
  # let exp2 = Main.parse_string "x / 10";;
  # Arith.eval_expr [("x", 500)] exp2;;
