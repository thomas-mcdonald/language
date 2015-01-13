open Check
open Gen
open Lexer
open Tree

let args = Arg.align [ "--ast", Arg.Set Config.print_ast, "Print the AST"; ]

let main () =
  let arg = ref [] in
  Arg.parse args (fun x -> arg := !arg @ [x]) "Usage:";
  let filename = List.hd !arg in
  let inch = open_in filename in
  let lexbuf = Lexing.from_channel inch in
  let prog = try Parser.program Lexer.token lexbuf with
    Parsing.Parse_error ->
      let tok = Lexing.lexeme lexbuf in
      let pointer = lexbuf.Lexing.lex_curr_p in
      let line = pointer.Lexing.pos_lnum in
      let col = pointer.Lexing.pos_cnum - pointer.Lexing.pos_bol in
      Printf.printf "parse error at token %s, (%d, %d)" tok line col; exit 1 in
  if !Config.print_ast then print_string (Print.program prog);
  Check.annotate prog;
  generate prog;
  exit 0

let language = main ()