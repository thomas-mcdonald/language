open Dict
open Keiko
open Printf
open Tree

(* temporary output method *)
let put (s : string) : unit =
  print_string (s ^ "\n");
  ()

let gen (w : icode) = printf "%s\n" (string_of_icode w)

(* get a list of all parents, Object at the top *)
let rec find_hierarchy (d : def) : def list =
  let cd = find_class_data d in
  match cd.c_super with
    Some(d') -> (find_hierarchy d') @ [d]
  | None -> [d]

(* generate the code for each proceedure in a class. This is initially iterated
  from the definition
  *)
let rec gen_proc (c : name) (stmt : stmt) =
  match stmt with
    ClassDecl(c, _, xs) ->
      List.iter (gen_proc c) xs
  | MethodDecl(n,args,xs) ->
    (* PROC name nargs fsize gcmap *)
    (* TODO: second argument is size of local variable space *)
    gen (PROC ((c.n_name ^ "." ^ n.n_name), 0, INT(Int32.zero)));
    gen END;
    put ""
  | Declare(t,e) -> ()
  | _ -> failwith "gen_proc"

(* generate a method descriptor given a method def *)
let gen_method_descriptor (d : def) : icode =
  match d.d_type with
    MethDef(md) -> WORD (SYM (md.m_receiver.d_name ^ "." ^ d.d_name))

(* generate a descriptor *)
let gen_descriptor (n: name) =
  put (sprintf "! Descriptor for %s" n.n_name);
  put (sprintf "DEFINE %s" n.n_name);
  match n.n_def.d_type with
    ClassDef(cd) ->
      (* print method list *)
      let print_meth = (fun m -> gen (gen_method_descriptor m)) in
      List.map print_meth cd.c_methods;
      (* print class hierarchy *)
      gen (DEFINE (n.n_name ^ ".%super"));
      List.map (fun c -> gen (WORD (SYM c.d_name))) (find_hierarchy n.n_def);
      put ""
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
      let dummy_name = { n_name = ""; n_def = { d_name = ""; d_type = UnknownDef; d_env = new_env () }} in
      List.iter (gen_proc dummy_name) xs;
      (* gen_stmts xs; *)
      List.iter gen_class_desc (extract_classes xs);
  ()
