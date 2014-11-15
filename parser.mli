type token =
  | NUMBER of (int)
  | IDENT of (string)
  | CLASS
  | COMMA
  | DEF
  | DOT
  | EQUALS
  | EOF
  | END
  | LPAREN
  | PLUS
  | RPAREN
  | SEMI
  | INT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
