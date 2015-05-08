{
open Keiko
open Lexing
open Parser
open Tree

let lineno = ref 1

let keywords =
  let t = Hashtbl.create 16 in
  Hashtbl.add t "bool" BOOL;
  Hashtbl.add t "class" CLASS;
  Hashtbl.add t "def" DEF;
  Hashtbl.add t "end" END;
  Hashtbl.add t "false" FALSE;
  Hashtbl.add t "int" INT;
  Hashtbl.add t "new" NEW;
  Hashtbl.add t "puts" PUTS;
  Hashtbl.add t "super" SUPER;
  Hashtbl.add t "this" THIS;
  Hashtbl.add t "true" TRUE;
  t

let lookup s =
  try Hashtbl.find keywords s with
    Not_found ->
      IDENT s

}

rule token =
  parse
    ['A'-'Z']['A'-'Z''a'-'z']* as s
                      { TYPE s }
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as s
                      { lookup s }
  | ['0'-'9']+ as s   { NUMBER (int_of_string s) }
  | ":"               { COLON }
  | ","               { COMMA }
  | "."               { DOT }
  | "="               { EQUALS }
  | "<"               { GT }
  | "("               { LPAREN }
  | "-"               { MINUS }
  | "+"               { PLUS }
  | ")"               { RPAREN }
  | [' ''\t']+        { token lexbuf }
  | "\n"              { token lexbuf }
  | eof               { EOF }
