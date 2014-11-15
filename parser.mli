type token =
  | NUMBER of (int)
  | IDENT of (string)
  | CLASS
  | DEF
  | EOF
  | END
  | SEMI

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
