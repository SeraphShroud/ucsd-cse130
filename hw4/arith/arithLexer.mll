{
open ArithParser
}
rule token = parse
  | [' ' '\t' '\r' '\n']     { token lexbuf } 
  | ['0'-'9']+ as l          { CONST (int_of_string l) }
  | ['a'-'z']['A'-'z']* as l { VAR l }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIVIDE }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | eof                      { EOF }
