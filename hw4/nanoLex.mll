{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | "true"                  { TRUE }
  | "false"                 { FALSE }
  | "let"                   { LET } 
  | "rec"                   { REC }
  | "="                     { EQ }
  | "in"                    { IN }
  | "fun"                   { FUN }
  | "->"                    { ARROW }
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }

  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { MUL }
  | "/"                     { DIV }
  | "<"                     { LT }
  | "<="                    { LE }
  | "!="                    { NE }
  | "&&"                    { AND }
  | "||"                    { OR }

  | "("                     { LPAREN }
  | ")"                     { RPAREN }

  | "["                     { LBRAC }
  | "]"                     { RBRAC }
  | "::"                    { COLONCOLON }
  | ";"                     { SEMI }

  | ['0'-'9']+ as inum      { Num(int_of_string inum) }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str 
                            { Id(str) }

  | eof                     { EOF }
  | _                       { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
