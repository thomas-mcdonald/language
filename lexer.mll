{
open Keiko
open Lexing
open Parser
open Tree

let lineno = ref 1

let keywords =
  let t = Hashtbl.create 16 in
  Hashtbl.add t "class" CLASS;
  Hashtbl.add t "def" DEF;
  Hashtbl.add t "end" END;
  Hashtbl.add t "int" INT;
  t

let lookup s =
  try Hashtbl.find keywords s with
    Not_found ->
      IDENT s

}

rule token =
  parse
      ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as s
                      { lookup s }
  | ['0'-'9']+ as s   { NUMBER (int_of_string s) }
  | ","               { COMMA }
  | "."               { DOT }
  | "="               { EQUALS }
  | "("               { LPAREN }
  | "+"               { PLUS }
  | ")"               { RPAREN }
  | [' ''\t']+        { token lexbuf }
  | "\n"              { token lexbuf }
  | eof               { EOF }
