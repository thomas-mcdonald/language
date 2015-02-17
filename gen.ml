open Dict
open Keiko
open Printf
open Tree

(* temporary output method *)
let put (s : string) : unit =
  printf "%s\n" s;
  ()

let gen (w : icode) = printf "%s\n" (string_of_icode w)

(* get a list of all parents, Object at the top *)
let rec find_hierarchy (d : def) : def list =
  let cd = find_class_data d in
  match cd.c_super with
    Some(d') -> (find_hierarchy d') @ [d]
  | None -> [d]

let gen_addr (e: expr)=
  match e.e_guts with
  | Ident(n) ->
    (* this only prints the offset atm *)
    let vd = find_var_data n.n_def in
    begin match vd.v_place with
    | MethodVar -> LOCAL vd.v_offset
    (* TODO: class variables. need to workout how references to self are handled *)
    | ClassVar -> CONST (0,0) (* not sure what this const defn is tho *)
    end

let gen_expr (e: expr) = ()

(* assignment generation *)
(*
 * perform the computation in e2
   find the location of e1
   store value on stack in location
*)
let gen_assign (e1: expr) (e2: expr) =
  gen_expr e2;
  gen_addr e1

let gen_stmt (s: stmt) =
  match s with
  | Assign(e1,e2) -> ignore (gen_assign e1 e2)
  | Declare(t,e) -> () (* don't do anything for declares *)
  | _ -> () (* failwith "gen_proc" *)

(* generate the code for each procedure in a class. This is initially iterated
  from the definition
  *)
let gen_proc (c : name) (stmt : stmt) =
  match stmt with
  | MethodDecl(n,args,xs) ->
    let md = find_meth_data n.n_def in
      (* PROC name nargs fsize gcmap *)
      gen (PROC ((c.n_name ^ "." ^ n.n_name), md.m_size, INT(Int32.zero)));
      List.iter gen_stmt xs;
      gen END;
      put ""
  | Declare(t,e) -> ()
  | _ -> failwith "gen_proc"

let gen_procs (klass : klass) =
  match klass with
    Klass(n,s,xs) -> List.iter (gen_proc n) xs

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
      put (sprintf "! size - %d" cd.c_size);
      (* print method list *)
      let print_meth = (fun m -> gen (gen_method_descriptor m)) in
      List.map print_meth cd.c_methods;
      (* print class hierarchy *)
      gen (DEFINE (n.n_name ^ ".%super"));
      List.map (fun c -> gen (WORD (SYM c.d_name))) (find_hierarchy n.n_def);
      put ""
  | _ -> failwith "gen_descriptor"

(* pull out the name from a class and call gen_descriptor on it *)
let gen_class_desc (k : klass) =
  match k with
    Klass(n, _, _) -> gen_descriptor n

let gen_entrypoint () =
  put "PROC Program.main 0 0 0";
  put "GLOBAL Main";
  put "END"

let generate (prog : program) =
  put "MODULE Program 0 0";
  (* put "IMPORT Lib 0"; *)
  put "ENDHDR";
  match prog with
    Prog(cs) ->
      List.iter gen_procs cs;
      gen_entrypoint ();
      List.iter gen_class_desc cs;
      ()
