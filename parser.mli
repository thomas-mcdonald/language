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
  | RPAREN
  | SEMI
  | MINUS
  | PLUS
  | BOOL
  | COLON
  | FALSE
  | INT
  | PUTS
  | SWITCH
  | SUPER
  | THIS
  | TRUE
  | WHEN

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
