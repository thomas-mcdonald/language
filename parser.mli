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
  | NEW
  | LPAREN
  | PLUS
  | RPAREN
  | SEMI
  | BOOL
  | FALSE
  | INT
  | PUTS
  | TRUE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
