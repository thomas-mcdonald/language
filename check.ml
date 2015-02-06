open Dict
open Tree

(* utility methods for extraction etc *)
let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let sizeof (t : type_data) =
  match t with
  | Int -> 4
  | Object(_) -> 4

let ensure_unique env x =
  if def_exists env x then error (x ^ " is already defined")

let expr_class (env: environment) (e: expr) =
  match e.e_type with
  | Object(s) -> find_def env s.n_name
  | _ -> failwith "expr_class"

let type_data_to_typed t =
  match t with
  | Int -> Integer
  | Object(r) ->
    let d = !r in
      Object { n_name = d.d_name; n_def = d }

(* add a method to a class record.
    We first check whether the method is already in the list - if we are overriding a method
    If we are then we rewrite it in place, else we append the method to the end of the list
*)
let add_method (c : def) (d : def) =
  let matcher = (fun x -> x.d_name = d.d_name)
  and cd = find_class_data c in
  if List.exists matcher cd.c_methods then
    cd.c_methods <- List.map (fun x -> if matcher x then d else x) cd.c_methods
  else
    cd.c_methods <- cd.c_methods @ [d]

(* add a variable to a class - populates with variable offset & updates class size *)
let add_variable (c : def) (name : string) (t : type_data) =
  let cd = find_class_data c in
  let v = { v_offset = cd.c_size; v_type = t } in
  let d = { d_name = name; d_type = VarDef(v); d_env = new_env () } in
    cd.c_size <- cd.c_size + (sizeof t);
    cd.c_variables <- cd.c_variables @ [d];
    add_def c.d_env name d

let populate_klass (env : environment) (klass : klass) : environment =
  match klass with
    Klass(n,s, xs) ->
      let name = n.n_name in
      ensure_unique env name;

      if not (class_exists env s.n_name)
      then error "superclass not defined"
      (* TODO: pull this logic out of Dict and into a method, works for now... *)
      else let env' = define_class env name s.n_name in
      n.n_def <- find_def env' name;
      env'

let populate_variable (dec : stmt) class_name env : environment =
  let c = find_def env class_name in
  match dec with
    Declare(Object(t), e) ->
      (match e.e_guts with
        Ident(s) ->
          let type_name = t.n_name in
          if not (class_exists env type_name) then
            error ("can't define variable of type " ^ type_name ^ " as class not defined");
          ensure_unique c.d_env s;
          let ct = find_def env type_name in
          add_variable c s (Object (ref ct));
          t.n_def <- ct;
          env
        | _ -> error "unsupported variable name")
  | Declare(p, e) ->
      match e.e_guts with
        Ident(s) ->
          ensure_unique c.d_env s;
          add_variable c s Int;
          env
  | _ -> error "check variable called with a non var"

let populate_variables env klass : environment =
  let statement_filter = (fun x -> match x with Declare(_) -> true | _ -> false) in
  match klass with
    Klass(n, _, xs) ->
      let var_accu = (fun env x -> populate_variable x n.n_name env) in
      let statements = List.filter statement_filter xs in
      List.fold_left var_accu env statements


let populate_method (meth : stmt) class_name env : environment =
  let c = find_def env class_name in
  match meth with
    MethodDecl(n, args, xs) ->
      let name = n.n_name
      and meth_data = { m_receiver = c } in
      let d = { d_name = name; d_type = MethDef(meth_data); d_env = new_env () } in
      ensure_unique c.d_env name;
      add_def env name d; (* TODO: work out wtf this is for - is it useful? *)
      add_method c d;
      env
  | _ -> error "check_method called with a non-method stmt"

(* add the parents methods to the class *)
let add_parent_methods env (n : name) (s : name) =
  let superclass = find_def env s.n_name in
  let cd = find_class_data n.n_def in
  cd.c_methods <- (find_class_data superclass).c_methods

let populate_methods env klass : environment =
  let method_filter = (fun x -> match x with MethodDecl(_) -> true | _ -> false) in
  match klass with
    Klass(n, s, xs) ->
      add_parent_methods env n s;
      let meth_accu = (fun env x -> populate_method x n.n_name env) in
      let statements = List.filter method_filter xs in
      List.fold_left meth_accu env statements

(*
  populate_class_info extracts information about the classes in the program 
    & adds it to the enviornment
  This is done in multiple passes.
    * Fold through the list of classes, gathering a list of class names and verifying
      * no duplicate names
      * superclasses are already defined
    * Generate list of instance variables
      * Check types exist
      * Names are not duplicate
    * Generate list of methods
      * Names are not duplicate
    TODO: look into fusing some of these methods together
*)
let populate_class_info (classes : klass list) (env : environment) : unit =
  let env' = List.fold_left populate_klass env classes in
  let env'' = List.fold_left populate_variables env' classes in
  ignore (List.fold_left populate_methods env'' classes)

let rec check_expr (env: environment) (e : expr) =
  match e.e_guts with
  | Number(x) -> e.e_type <- Integer
  | Ident(x) ->
    let d = find_def env x in
    begin match d.d_type with
    | VarDef(v) ->
      e.e_type <- type_data_to_typed v.v_type
    | _ -> error "identifier does not refer to a variable"
    end
  | Binop(Plus, e1, e2) ->
    check_expr env e1;
    check_expr env e2;
    if e1.e_type <> Integer || e2.e_type <> Integer then error "plus only works on integers";
    e.e_type <- e1.e_type (* will be an integer*)
  | _ -> ()

(* check_assign ensures that the lhs is an identifier and that the types match up *)
let check_assign (env: environment) (e1: expr) (e2: expr) =
  match e1.e_guts with
  | Ident(i) ->
    check_expr env e1;
    check_expr env e2;
    if e1.e_type <> e2.e_type then error "assignment types mismatched"
  | _ -> () (* only identifiers are allowed in the parser *)

let rec check_stmt (env: environment) (s: stmt) =
  match s with
  | MethodDecl(_,_,xs) -> List.iter (check_stmt env) xs
  | Assign(e1, e2) -> check_assign env e1 e2
  | Declare(_,_) -> ()
  | Expr(e) -> check_expr env e

let check_class (env : environment) (klass : klass) =
  match klass with
  Klass(n,_,xs) ->
      List.iter (check_stmt n.n_def.d_env) xs

let annotate (program : program) : unit =
  let env = initial_env in
  match program with
    Prog(cs) ->
      populate_class_info cs env;
      List.iter (check_class env) cs
