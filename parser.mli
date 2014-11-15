type token =
  | NUMBER of (int)
  | IDENT of (string)
  | CLASS
  | DEF
  | EQUALS
  | EOF
  | END
  | SEMI
  | INT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
