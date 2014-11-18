let put (s : string) : unit =
  print_string (s ^ "\n");
  ()

let generate (prog : Tree.program) : unit =
  put "MODULE Main 0 0";
  put "IMPORT Lib 0";
  put "ENDHDR";
  ()
