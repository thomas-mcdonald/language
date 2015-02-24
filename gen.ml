open Dict
open Keiko
open Printf
open Tree

(* temporary output method *)
let put (s : string) : unit =
  printf "%s\n" s;
  ()

let rec gen (w : icode) =
  match w with
  | SEQ xs -> List.iter gen xs
  | _ -> printf "%s\n" (string_of_icode w)

(* get a list of all parents, Object at the top *)
let rec find_hierarchy (d : def) : def list =
  let cd = find_class_data d in
  match cd.c_super with
    Some(d') -> (find_hierarchy d') @ [d]
  | None -> [d]

let gen_addr (e: expr) : icode =
  match e.e_guts with
  | Ident(n) ->
    (* this only prints the offset atm *)
    let vd = find_var_data n.n_def in
    begin match vd.v_place with
    | MethodVar -> LOCAL vd.v_offset
    | ClassVar ->
      SEQ [
        LOCAL 16; (* object location is first argument *)
        LOADW;
        CONST vd.v_offset;
        BINOP Plus;
      ]
    end
  | _ -> failwith "gen_addr"

(* gen_method_call generates the address of a method *)
let gen_method_addr (e: expr) (t: typed) : icode =
  match e.e_guts with
  | Ident(n) ->
    begin match t with
    | Object(s) ->
        GLOBAL (sprintf "%s.%s" s.n_name n.n_name)
    | Unknown -> failwith "type failure"
    | _ -> failwith "gen_method_addr"
    end
  | _ -> failwith "gen_method_addr"

let rec gen_expr (e: expr) : icode =
  match e.e_guts with
  | Binop(op, e1, e2) ->
    SEQ [gen_expr e1; gen_expr e2; BINOP(op)]
  | Call(e1, e2, es) ->
    (* e1 = object instance *)
    (* e2 = method - ident *)
    SEQ [
      gen_addr e1;
      CONST 0;
      gen_method_addr e2 e1.e_type;
      PCALL 1; (* TODO: needs parameterizing *)
    ]
  | Ident(_) -> SEQ [gen_addr e; LOADW]
  | New(t) ->
    begin match t with
    | Object(n) ->
      let cd = find_class_data n.n_def in
        SEQ [
          CONST cd.c_size;
          GLOBAL n.n_name;
          CONST 0;
          GLOBAL "Lib.New";
          PCALL 2;
        ]
    | _ -> failwith "gen_expr#new"
    end
  | Number(x) -> CONST x
  | Puts(e) -> SEQ [gen_expr e; CONST 0; GLOBAL "Lib.Print"; PCALL 1]
  | _ -> SEQ []

(* assignment generation *)
(*
 * perform the computation in e2
   find the location of e1
   store value on stack in location
*)
let gen_assign (e1: expr) (e2: expr) : icode =
  SEQ [gen_expr e2; gen_addr e1; STOREW]

let gen_stmt (s: stmt) : icode =
  match s with
  | Assign(e1,e2) -> gen_assign e1 e2
  | Declare(t,e) -> SEQ [] (* don't do anything for declares *)
  | Expr(e) -> gen_expr e
  | _ -> SEQ [] (* failwith "gen_proc" *)

(* generate the code for each procedure in a class. This is initially iterated
  from the definition
  *)
let gen_proc (c : name) (stmt : stmt) : icode =
  match stmt with
  | MethodDecl(n,args,xs) ->
    let md = find_meth_data n.n_def in
      (* PROC name nargs fsize gcmap *)
      SEQ [
        PROC ((c.n_name ^ "." ^ n.n_name), md.m_size, INT(Int32.zero));
        SEQ (List.map gen_stmt xs);
        RETURN;
        END;
      ]
  | Declare(t,e) -> SEQ []
  | _ -> failwith "gen_proc"

let gen_procs (klass : klass) =
  match klass with
    Klass(n,s,xs) -> gen (SEQ (List.map (gen_proc n) xs))

(* generate a method descriptor given a method def *)
let gen_method_descriptor (d : def) : icode =
  match d.d_type with
  | MethDef(md) -> WORD (SYM (md.m_receiver.d_name ^ "." ^ d.d_name))
  | _ -> failwith "gen_method_descriptor"

(* generate a descriptor *)
let gen_descriptor (n: name) =
  match n.n_def.d_type with
    ClassDef(cd) ->
      let print_meth m = gen_method_descriptor m in
      gen (SEQ [
        COMMENT (sprintf "Descriptor for %s" n.n_name);
        DEFINE n.n_name;
        COMMENT (sprintf "size - %d" cd.c_size);
        (* print method list *)
        SEQ (List.map print_meth cd.c_methods);
        (* print class hierarchy *)
        DEFINE (n.n_name ^ ".%super");
        SEQ (List.map (fun c -> WORD (SYM c.d_name)) (find_hierarchy n.n_def));
        NEWLINE
      ]);
  | _ -> failwith "gen_descriptor"

(* pull out the name from a class and call gen_descriptor on it *)
let gen_class_desc (k : klass) =
  match k with
    Klass(n, _, _) -> gen_descriptor n

let gen_entrypoint main =
  let cd = find_class_data main in
  put "PROC Program.%main 0 0 0";
  gen (SEQ [
    (* create a main object, address goes on stack *)
    CONST cd.c_size;
    GLOBAL "Main";
    CONST 0;
    GLOBAL "Lib.New";
    PCALL 2;
    (* now call the main method on that object *)
    CONST 0;
    GLOBAL "Main.main";
    PCALL 1;
    RETURN;
    END;
    NEWLINE; (* add a couple line gap *)
  ])

let generate (prog : program) =
  let main_match c = match c with Klass(n,_,_) -> n.n_name = "Main"
  and extract_def c = match c with Klass(n,_,_) -> n.n_def in
  put "MODULE Program 0 0";
  put "ENDHDR";
  match prog with
    Prog(cs) ->
      let mainclass = extract_def (List.find main_match cs) in
      List.iter gen_procs cs;
      gen_entrypoint mainclass;
      gen (DEFINE "Object"); (* TODO: this is hacky *)
      List.iter gen_class_desc cs;
      gen NEWLINE;
      ()
