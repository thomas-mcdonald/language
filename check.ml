open Dict
open Keiko
open Printf
open Tree

(* utility methods for extraction etc *)
let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let sizeof (t : type_data) =
  match t with
  | Bool -> 1
  | Int -> 4
  | Object(_) -> 4

let differing_type x y =
  match x,y with
  | Object(s), Object(t) ->
    t.n_name <> s.n_name
  | _ -> x <> y

(* top level environment *)
let top_env = ref initial_env

let ensure_unique env x =
  if def_exists env x then error (x ^ " is already defined")

let expr_class (env: environment) (e: expr) =
  match e.e_type with
  | Object(s) -> find_def env s.n_name
  | _ -> failwith "expr_class"

let type_data_to_typed (t: type_data) : typed =
  match t with
  | Bool -> Bool
  | Int -> Integer
  | Void -> Unknown
  | Object(r) ->
    let d = !r in
      Object { n_name = d.d_name; n_def = d }

let add_argument_variable (m: def) (name: string) (t: type_data) =
  let md = find_meth_data m in
  let v = { v_offset = (md.m_arg_count + 1) * 4; v_type = t; v_place = FunctionArg } in
  let d = { d_name = name; d_type = VarDef(v); d_env = new_env () } in
    md.m_args <- md.m_args @ [d];
    md.m_arg_count <- md.m_arg_count + 1;
    ignore (add_def m.d_env name d)

(* add a local method variable to the method data *)
let add_method_variable (m: def) (name: string) (t: type_data) =
  let md = find_meth_data m in
  let v = { v_offset = md.m_size; v_type = t; v_place = MethodVar } in
  let d = { d_name = name; d_type = VarDef(v); d_env = new_env () } in
    md.m_size <- md.m_size + (sizeof t);
    ignore (add_def m.d_env name d)

(* add a method to a class record.
    We first check whether the method is already in the list - if we are overriding a method
    If we are then we rewrite it in place (and set the new m_offset appropriately), else we
    append the method to the end of the list
*)
let add_method (c : def) (d : def) =
  let matcher x = x.d_name = d.d_name
  and offset d = (find_meth_data d).m_offset
  and set_offset d v = (find_meth_data d).m_offset <- v; d in
  let searcher x = if matcher x then set_offset d (offset x) else x
  and cd = find_class_data c in
    if List.exists matcher cd.c_methods then
      cd.c_methods <- List.map searcher cd.c_methods
    else
      cd.c_methods <- cd.c_methods @ [set_offset d ((List.length cd.c_methods + 1) * 4)]

(* add a variable to a class - populates with variable offset & updates class size *)
let add_variable (c : def) (name : string) (t : type_data) =
  let cd = find_class_data c in
  let v = { v_offset = cd.c_size; v_type = t; v_place = ClassVar } in
  let d = { d_name = name; d_type = VarDef(v); d_env = new_env () } in
    cd.c_size <- cd.c_size + (sizeof t);
    cd.c_variables <- cd.c_variables @ [d];
    ignore (add_def c.d_env name d)

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
      s.n_def <- find_def env' s.n_name;
      env';;

let add_primitive_variable (fn: def -> string -> type_data -> unit) (p: typed) (d: def) (n: name) =
  match p with
  | Bool -> fn d n.n_name Bool
  | Integer -> fn d n.n_name Int
  | _ -> error "unsupported primitive type"

let populate_method_argument (arg: method_arg) (m: name) (env: environment) =
  let d = m.n_def in
  match arg with
  | Arg(n, Object(t)) ->
    let type_name = t.n_name in env (* TODO!! *)
  | Arg(n, p) ->
    ensure_unique d.d_env n.n_name;
    add_primitive_variable add_argument_variable p d n;
    env

let populate_method_variable (dec: stmt) (m: name) env : environment =
  let d = m.n_def in
  match dec with
  | Declare(Object(t), e) ->
    begin match e.e_guts with
    | Ident(n) ->
      let type_name = t.n_name in
      if not (class_exists env type_name) then
        error ("can't define variable of type " ^ type_name ^ " as class not defined");
      ensure_unique d.d_env n.n_name;
      let ct = find_def env type_name in
        add_method_variable d n.n_name (Object (ref ct));
        t.n_def <- ct;
        env
    | _ -> error "unsupported variable name"
    end
  | Declare(p, e) ->
    begin match e.e_guts with
    | Ident(n) ->
      ensure_unique d.d_env n.n_name;
      begin match p with
      | Bool -> add_method_variable d n.n_name Bool
      | Integer -> add_method_variable d n.n_name Int
      | _ -> error "other primitive type reached populate_variable"
      end;
      env
    | _ -> error "unsupported variable name"
    end
  | _ -> error "check variable called with a non var"

let populate_class_variable (dec : stmt) (klass: name) env : environment =
  let c = find_def env klass.n_name in
  match dec with
  | Declare(Object(t), e) ->
    begin match e.e_guts with
    | Ident(n) ->
      let type_name = t.n_name in
      if not (class_exists env type_name) then
        error ("can't define variable of type " ^ type_name ^ " as class not defined");
      ensure_unique c.d_env n.n_name;
      let ct = find_def env type_name in
        add_variable c n.n_name (Object (ref ct));
        t.n_def <- ct;
        env
    | _ -> error "unsupported variable name"
    end
  | Declare(p, e) ->
    begin match e.e_guts with
    | Ident(n) ->
      ensure_unique c.d_env n.n_name;
      begin match p with
      | Bool -> add_variable c n.n_name Bool
      | Integer -> add_variable c n.n_name Int
      | _ -> error "other primitive type reached populate_variable"
      end;
      env
    | _ -> error "unsupported variable name"
    end
  | _ -> error "check variable called with a non var"

let populate_variables env klass : environment =
  let statement_filter x = match x with Declare(_) -> true | _ -> false in
  match klass with
  | Klass(n, s, xs) ->
    let var_accu env x = populate_class_variable x n env
    and statements = List.filter statement_filter xs
    and scd = find_class_data s.n_def
    and cd = find_class_data n.n_def in
      (* copy over variables from superclass *)
      cd.c_size <- scd.c_size;
      cd.c_variables <- scd.c_variables;
      let def_adder v = ignore (add_def n.n_def.d_env v.d_name v) in
      List.iter def_adder cd.c_variables;
      (* add new variables *)
      List.fold_left var_accu env statements

let populate_method (meth: stmt) class_name env : environment =
  let statement_filter x = match x with Declare(_) -> true | _ -> false
  and already_defined n = Printf.sprintf "%s is already defined" n
  and class_compare a b = a.d_name = b.d_name
  and c = find_def env class_name in
  match meth with
    MethodDecl(n, args, xs, r) ->
      let name = n.n_name
      and r_type =
        begin match r with
        | Unknown -> Void
        | Integer -> Int
        | Bool -> Bool
        | Object(n) -> Object(ref (find_def !top_env n.n_name))
        end in
      let meth_data = { m_receiver = c; m_return = r_type; m_size = 0;
        m_offset = 0; m_arg_count = 0; m_args = []; m_super_rec = None }
      and argument_acc env x = populate_method_argument x n env
      and variable_acc env x = populate_method_variable x n env in
      let d = { d_name = name; d_type = MethDef(meth_data); d_env = new_env () } in
        n.n_def <- d;
        ignore @@ add_def d.d_env "__method" d;
        ignore @@ List.fold_left variable_acc env (List.filter statement_filter xs);
        ignore @@ List.fold_left argument_acc env args;
        (* check if the method has been defined in this class already *)
        begin try
          let old = find_def c.d_env name in
          let md = find_meth_data old in
          if class_compare md.m_receiver meth_data.m_receiver
            then error (already_defined name);
          (* method is from inheritance, overwrite it  *)
          ignore (remove_def c.d_env name)
        with
        | Not_found -> ()
        | Not_method ->
          error (already_defined name)
        end;
        add_method c d;
        ignore @@ add_def c.d_env name d;
        env
  | _ -> error "check_method called with a non-method stmt"

(* add the parents methods to the class *)
let add_parent_methods env (n : name) (s : name) =
  let superclass = find_def env s.n_name in
  let cd = find_class_data n.n_def in
  let method_adder m = ignore (add_def n.n_def.d_env m.d_name m) in
  let scd = find_class_data superclass in
  List.iter method_adder scd.c_methods;
  cd.c_methods <- scd.c_methods

let populate_methods env klass : environment =
  let method_filter x = match x with MethodDecl(_) -> true | _ -> false in
  match klass with
    Klass(n, s, xs) ->
      ignore (add_def n.n_def.d_env "super" s.n_def);
      ignore (add_def n.n_def.d_env "this" n.n_def);
      add_parent_methods env n s;
      let meth_accu env x = populate_method x n.n_name env
      and statements = List.filter method_filter xs in
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
let populate_class_info (classes : klass list) (env : environment) : environment =
  let env' = List.fold_left populate_klass env classes in
  let env'' = List.fold_left populate_variables env' classes in
  List.fold_left populate_methods env'' classes

let check_method_call (e': expr) (t: typed) (e: expr) =
  let cenv = match t with Object(n) -> n.n_def.d_env | _ -> failwith "check_method_call#find_type" in
  match e.e_guts with
  | Ident(n) ->
    begin try
      let d = find_def cenv n.n_name in
      let md = find_meth_data d in
      e'.e_type <- type_data_to_typed md.m_return;
      n.n_def <- d;
    with Not_found ->
      failwith (sprintf "method %s does not exist" n.n_name)
    end
  | _ -> failwith "check_method_call"

let check_args (x: def * expr) =
  let (d,e) = x in
  let vd = find_var_data d in
  if differing_type (type_data_to_typed vd.v_type) e.e_type then error "argument types differ"

let rec check_whence (cenv: environment) (menv: environment) (x : whence) =
  match x with
  | When(t, xs) -> List.iter (check_stmt cenv menv) xs

and check_expr (cenv: environment) (menv: environment) (e : expr) =
  match e.e_guts with
  | Number(x) -> e.e_type <- Integer
  | Boolean(x) -> e.e_type <- Bool
  | This -> e.e_type <-
    let d = find_def cenv "this" in
    let n = { n_name = d.d_name; n_def = d } in
      Object(n)
  | Ident(n) ->
    let search_environment env failure =
      begin try
        let d = find_def env n.n_name in
        n.n_def <- d;
        begin match d.d_type with
        | VarDef(v) ->
          e.e_type <- type_data_to_typed v.v_type
        | _ -> error (sprintf "identifier %s does not refer to a variable" n.n_name)
        end
      with Not_found ->
        failure ()
      end in
    search_environment menv (fun () ->
      search_environment cenv (fun () ->
        error (sprintf "%s is not a variable" n.n_name)))
  | Binop(Plus, e1, e2) ->
    check_expr cenv menv e1;
    check_expr cenv menv e2;
    if e1.e_type <> Integer || e2.e_type <> Integer then error "plus only works on integers";
    e.e_type <- e1.e_type (* will be an integer*)
  | Call(e1, e2, es) ->
    check_expr cenv menv e1;
    check_method_call e e1.e_type e2;
    begin try match e2.e_guts with
    | Ident(n) ->
      let md = find_meth_data n.n_def in
        List.iter (check_expr cenv menv) es;
        List.iter check_args (List.combine md.m_args es);
    | _ -> failwith "check_expr#call"
    with Invalid_argument(_) ->
      error "different number of arguments"
    end
  | New(t) ->
    begin match t with
    | Object(n) ->
      let d = find_def !top_env n.n_name in
      e.e_type <- type_data_to_typed (Object (ref d));
      n.n_def <- d
    | _ -> failwith "check_expr new"
    end
  | Puts(e) -> check_expr cenv menv e;
  | Super ->
    let c = find_def cenv "this" and
        d = find_def menv "__method" in
    let method_matcher klass =
      begin try
        let d = find_def klass.d_env d.d_name in
        (* ensure that we only consider the original definitions *)
        (find_meth_data d).m_receiver.d_name = klass.d_name
      with Not_found -> false
    end in
    (* find the class this method is next defined on *)
    begin try
      let sc = List.find method_matcher (List.tl (find_hierarchy c)) in
        e.e_type <-
          let d = find_def sc.d_env d.d_name in
          let md = find_meth_data d in
          md.m_super_rec <- Some(sc);
          let t = { n_name = d.d_name; n_def = d } in Object(t)
    with Not_found -> error @@ sprintf "super method %s not defined" d.d_name
    end
  | Switch(e, xs) ->
    check_expr cenv menv e;
    List.iter (check_whence cenv menv) xs
  | _ -> ()

(* check_assign ensures that the lhs is an identifier and that the types match up *)
(* t is an optional typecast *)
and check_assign (cenv: environment) (menv: environment) (e1: expr) (t : typed) (e2: expr) =
  (* required to simplify comparison of typed objects *)
  let unknown = match t with Unknown -> true | _ -> false and
      update_def t = match t with Object(x) -> x.n_def <- find_def !top_env x.n_name; t in
  let assignment_type e = if unknown then e.e_type else (update_def t) in
  match e1.e_guts with
  | Ident(i) ->
    check_expr cenv menv e1;
    check_expr cenv menv e2;
    begin match e1.e_type, (assignment_type e2) with
    | Object(x), Object(y) ->
      if List.mem x.n_def (find_hierarchy y.n_def) then () else error "assignment types mismatched"
    | x, y -> if x <> y then error "assignment types mismatched"
    end
  | _ -> () (* only identifiers are allowed in the parser *)

(* we call this if the function has a return value.
  if the Invalid_argument block is called then the list of statements is empty
  eg the function doesn't return anything, resulting in an error
 *)
and check_return_value (n: name) (xs: stmt list) (r: typed) =
  begin try match List.nth xs ((List.length xs) - 1) with
  |  Expr(e) ->
    if differing_type e.e_type r then error "last expression must match the return type"
  | _ -> error "last expression must match the return type"
  with Invalid_argument(_) ->
    error (sprintf "function %s has an empty body, cannot return" n.n_name)
  end

and check_stmt (cenv: environment) (menv: environment) (s: stmt) =
  match s with
  | MethodDecl(n,_,xs, r) ->
    List.iter (check_stmt cenv n.n_def.d_env) xs;
    if r <> Unknown then check_return_value n xs r
  | Assign(e1, t, e2) -> check_assign cenv menv e1 t e2
  | Declare(_,_) -> ()
  | Expr(e) -> check_expr cenv menv e

let check_class (env : environment) (klass : klass) =
  match klass with
  Klass(n,_,xs) ->
      List.iter (check_stmt n.n_def.d_env (new_env ())) xs

let annotate (program : program) : unit =
  let env = !top_env in
  match program with
    Prog(cs) ->
      top_env := populate_class_info cs env;
      List.iter (check_class env) cs
