type token =
  | Num of (int)
  | EOF
  | Id of (string)
  | TRUE
  | FALSE

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
