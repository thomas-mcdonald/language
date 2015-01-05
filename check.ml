open Dict
open Tree

let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let ensure_unique env x =
  if def_exists env x then error (x ^ " is already defined")

(* add a method to a class record.  *)
let add_method (c : def) (d : def) =
  let cd = find_class_data c in
  cd.c_methods <- cd.c_methods @ [d]

let populate_stmt (env : environment) (stmt : stmt) : environment =
  match stmt with
    (* class declaration without superclass *)
    ClassDecl(n, Void, _) ->
      let name = n.n_name in
      ensure_unique env name;
      (* TODO: pull this logic out of Dict and into a method, works for now... *)
      let env' = define_class env name "" in
      n.n_def <- find_def env' name;
      env'
    (* class declaration with superclass *)
  | ClassDecl(n, Object(s), _) ->
      let name = n.n_name in
      ensure_unique env name;
      if not (class_exists env s)
      then error "superclass not defined"
      else let env' = define_class env name s in
      n.n_def <- find_def env' name;
      env'

let populate_variable (dec : stmt) class_name env : environment =
  let c = find_def env class_name in
  match dec with
    Declare(Object(t), e) ->
      (match e.e_guts with
        Ident(s) ->
          if not (class_exists env t) then
            error ("can't define variable of type " ^ t ^ " as class not defined");
          ensure_unique c.d_env s;
          let ct = find_def env t in
          define_variable c.d_env s ct;
          env
        | _ -> error "unsupported variable name")
  | Declare(p, x) ->
      error "declare called with a primitive - TODO"
  | _ -> error "check variable called with a non var"

let populate_variables env klass : environment =
  let statement_filter = (fun x -> match x with Declare(_) -> true | _ -> false) in
  match klass with
    ClassDecl(n, _, xs) ->
      let var_accu = (fun env x -> populate_variable x n.n_name env) in
      let statements = List.filter statement_filter xs in
      List.fold_left var_accu env statements
  | _ -> error "check_variables called against a non-class"


let populate_method (meth : stmt) class_name env : environment =
  let c = find_def env class_name in
  match meth with
    MethodDecl(n, args, xs) ->
      let name = n.n_name in
      let d = { d_name = name; d_type = MethDef; d_env = new_env () } in
      ensure_unique c.d_env name;
      add_def env name d; (* TODO: work out wtf this is for - is it useful? *)
      add_method c d;
      env
  | _ -> error "check_method called with a non-method stmt"
  env

let populate_methods env klass : environment =
  let method_filter = (fun x -> match x with MethodDecl(_) -> true | _ -> false) in
  match klass with
    ClassDecl(n, _, xs) ->
      let meth_accu = (fun env x -> populate_method x n.n_name env) in
      let statements = List.filter method_filter xs in
      List.fold_left meth_accu env statements
    | _ -> error "check_methods called against a non-class"

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
let populate_class_info (classes : stmt list) (env : environment) =
  let env' = List.fold_left populate_stmt env classes in
  let env'' = List.fold_left populate_variables env' classes in
  List.fold_left populate_methods env'' classes

let check_block (env : environment) (block : block) =
  match block with
      NoBlock -> raise (Failure "no program given")
    | Block(xs) -> populate_class_info xs env

let annotate (program : program) : unit =
  let env = initial_env in
  match program with
    Prog(block) ->
      check_block env block;
      ()
