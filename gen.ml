open Tree

let put (s : string) : unit =
  print_string (s ^ "\n");
  ()

let gen_stmt (stmt : stmt) =
  match stmt with
    ClassDecl(e, xs) ->
      put "! Class Description";
      (* class code *)
      ()

let gen_stmts (xs : stmt list) =
  List.map gen_stmt xs

let generate (prog : program) : unit =
  put "MODULE Main 0 0";
  put "IMPORT Lib 0";
  put "ENDHDR";
  match prog with
    Prog(Block (xs)) ->
      gen_stmts xs;
  ()
