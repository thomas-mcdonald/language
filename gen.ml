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

let gen_addr (e: expr) : icode =
  match e.e_guts with
  | Ident(n) ->
    let vd = find_var_data n.n_def in
    begin match vd.v_place with
    | MethodVar -> LOCAL (- (vd.v_offset + 4))
    | ClassVar ->
      SEQ [
        LOCAL 12; (* object location is first argument *)
        LOADW;
        CONST vd.v_offset;
        BINOP Plus;
      ]
    | FunctionArg -> LOCAL (12 + vd.v_offset)
    end
  | This -> SEQ [ LOCAL 12; ]
  | _ -> failwith "gen_addr"

(* gen_method_call generates the address of a method *)
let gen_method_addr (e1: expr) (e2: expr) : icode =
  match e2.e_guts with
  | Ident(n) ->
    let md = find_meth_data n.n_def in
    begin match e1.e_type with
    | Object(s) ->
      SEQ [
        (* load the class descriptor at position 0 and use this to resolve the correct method *)
        COMMENT (sprintf "method address for %s.%s" s.n_name n.n_name);
        gen_addr e1; (* address of object *)
        LOADW; (* memory location of object *)
        LOADW; (* memory location of descriptor *)
        CONST md.m_offset;
        BINOP Plus;
        LOADW;
      ]
    | Unknown -> failwith "type failure"
    | _ -> failwith "gen_method_addr"
    end
  | _ -> failwith "gen_method_addr"

let gen_obj_call_addr e =
  let is_primitive t = match t with Object(_) -> false | _ -> true in
  if is_primitive e.e_type then gen_addr e
  else SEQ [gen_addr e; LOADW; ]

let rec gen_expr (e: expr) : icode =
  match e.e_guts with
  | Binop(op, e1, e2) ->
    SEQ [gen_expr e1; gen_expr e2; BINOP(op)]
  | Call(e1, e2, es) ->
    (* e1 = object instance *)
    (* e2 = method - ident *)
    let extract_name x = match x with Ident(n) -> n.n_def | _ -> failwith "extract_name" in
    let md = find_meth_data (extract_name e2.e_guts) in
    let arg_count = md.m_arg_count + 1 in
    let call_code = if md.m_return <> Void then CALLW arg_count else CALL arg_count in
      SEQ [
        SEQ (List.map gen_expr es);
        gen_obj_call_addr e1;
        gen_method_addr e1 e2;
        call_code;
      ]
  | Ident(_) -> SEQ [gen_addr e; LOADW]
  | New(t) -> failwith "gen_expr#new" (* shouldn't be called, means we are
                                        creating a new obj outside of assign *)
  | Number(x) -> CONST x
  | Puts(e) -> SEQ [gen_expr e; GLOBAL "Lib.Print"; CALL 1]
  | _ -> SEQ []

let gen_new_assign (e: expr) (t: typed) =
  match t with
  | Object(n) ->
    let cd = find_class_data n.n_def in
      SEQ [
        CONST cd.c_size;
        GLOBAL n.n_name;
        gen_addr e;
        GLOBAL "Lib.New";
        CALL 3;
      ]

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
  | Assign(e1,e2) ->
    begin match e2.e_guts with
    | New(t) -> gen_new_assign e1 t
    | _ -> gen_assign e1 e2
    end
  | Declare(t,e) -> SEQ [] (* don't do anything for declares *)
  | Expr(e) -> gen_expr e
  | _ -> SEQ [] (* failwith "gen_proc" *)

(* generate the code for each procedure in a class. This is initially iterated
  from the definition
  *)
let gen_proc (c : name) (stmt : stmt) : icode =
  match stmt with
  | MethodDecl(n,args,xs,r) ->
    let md = find_meth_data n.n_def
    and return_code = if r <> Unknown then RETURNW else RETURN in
      (* PROC name nargs fsize gcmap *)
      SEQ [
        PROC ((c.n_name ^ "." ^ n.n_name), md.m_size, INT(Int32.zero));
        SEQ (List.map gen_stmt xs);
        return_code;
        END;
      ]
  | Declare(t,e) -> SEQ []
  | _ -> failwith "gen_proc"

let gen_procs (klass : klass) =
  match klass with
    Klass(n,s,xs) -> SEQ (List.map (gen_proc n) xs)

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
      SEQ [
        COMMENT (sprintf "Descriptor for %s" n.n_name);
        DEFINE n.n_name;
        WORD (INT Int32.zero);
        (* print method list *)
        SEQ (List.map print_meth cd.c_methods);
        (* print class hierarchy *)
        DEFINE (n.n_name ^ ".%super");
        SEQ (List.map (fun c -> WORD (SYM c.d_name)) (find_hierarchy n.n_def));
        NEWLINE
      ];
  | _ -> failwith "gen_descriptor"

(* pull out the name from a class and call gen_descriptor on it *)
let gen_class_desc (k : klass) =
  match k with
    Klass(n, _, _) -> gen_descriptor n

let gen_entrypoint main =
  let cd = find_class_data main in
  SEQ [
    STRING "PROC Program.%main 4 0 0";
    (* create a main object, address goes in local var *)
    CONST cd.c_size;
    GLOBAL "Main";
    LOCAL (-4);
    GLOBAL "Lib.New";
    CALL 3;
    (* now call the main method on that object *)
    LOCAL (-4); LOADW;
    GLOBAL "Main.main";
    CALL 1;
    RETURN;
    END;
    NEWLINE; (* add a couple line gap *)
  ]

let generate (prog : program) =
  let main_match c = match c with Klass(n,_,_) -> n.n_name = "Main"
  and extract_def c = match c with Klass(n,_,_) -> n.n_def
  and header = SEQ [
    STRING "MODULE Program 0 0";
    STRING "ENDHDR\n"
  ] in
  match prog with
    Prog(cs) ->
      let mainclass = extract_def (List.find main_match cs) in
      gen (SEQ [
        header;
        SEQ (List.map gen_procs cs);
        gen_entrypoint mainclass;
        DEFINE "Object"; (* TODO: this is hacky *)
        SEQ (List.map gen_class_desc cs);
        NEWLINE
      ]);
      ()
