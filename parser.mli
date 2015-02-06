type token =
  | NUMBER of (int)
  | IDENT of (string)
  | TYPE of (string)
  | CLASS
  | COMMA
  | DEF
  | DOT
  | EQUALS
  | EOF
  | END
  | GT
  | LPAREN
  | PLUS
  | RPAREN
  | SEMI
  | FALSE
  | INT
  | TRUE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
