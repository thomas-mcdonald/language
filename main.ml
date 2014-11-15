open Lexer
open Tree

let main () =
  let argv = Array.to_list Sys.argv in
  if List.length argv <> 2 then begin
    print_string "usage: ./language [FILE]\n"; exit 1;
  end;
  let filename = List.nth argv 1 in
  let inch = open_in filename in
  let lexbuf = Lexing.from_channel inch in
  let prog = try Parser.program Lexer.token lexbuf with
    Parsing.Parse_error ->
      print_string "parse error"; exit 1 in
  print_string (ppProg prog);
  exit 0

let language = main ()