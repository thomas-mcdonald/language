open Dict
open Keiko
open Printf
open Tree

(* temporary output method *)
let put (s : string) : unit =
  print_string (s ^ "\n");
  ()

let icode_print (w : icode) : string =
  match w with
    DEFINE(s) -> "DEFINE " ^ s
  | WORD(SYM(s)) -> "WORD " ^ s

let gen (w : icode) = printf "%s\n" (icode_print w)

(* get a list of all parents, Object at the top *)
let rec find_hierarchy (d : def) : def list =
  let cd = find_class_data d in
  match cd.c_super with
    Some(d') -> (find_hierarchy d') @ [d]
  | None -> [d]

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
    ClassDef(cd) ->
      (* print method list *)
      let print_meth = (fun m -> gen (WORD (SYM (n.n_name ^ "." ^ m.d_name)))) in
      List.map print_meth cd.c_methods;
      (* print class hierarchy *)
      gen (DEFINE (n.n_name ^ ".%super"));
      List.map (fun c -> gen (WORD (SYM c.d_name))) (find_hierarchy n.n_def)
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
