open Dict
open Printf
open Tree

(* temporary output method *)
let put (s : string) : unit =
  print_string (s ^ "\n");
  ()

let gen_stmt (stmt : stmt) =
  match stmt with
    ClassDecl(e, e', xs) ->
      put "! Class Description";
      (* class code *)
      ()

let gen_stmts (xs : stmt list) =
  List.map gen_stmt xs

(* generate a descriptor *)
let gen_descriptor (n: name) =
  put (sprintf "! Descriptor for %s" n.n_name);
  put (sprintf "DEFINE %s" n.n_name);
  match n.n_def.d_type with
    ClassDef(s, cd) ->
      let print_meth = (fun m -> put (sprintf "WORD %s.%s" n.n_name m.d_name)) in
      List.map print_meth cd.c_methods
  | _ -> failwith "gen_descriptor"

(* pull out the name from a class and call gen_descriptor on it *)
let gen_class_desc (x : stmt) =
  match x with
    ClassDecl(n, _, _) -> gen_descriptor n

let generate (prog : program) =
  put "MODULE Main 0 0";
  put "IMPORT Lib 0";
  put "ENDHDR";
  match prog with
    Prog(Block (xs)) ->
      gen_stmts xs;
      List.map gen_class_desc (extract_classes xs);
  ()
