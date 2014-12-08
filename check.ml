open Dict
open Tree

let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let ensure_unique env x =
  if def_exists env x then error (x ^ " is already defined")

(* populate_class checks that the class name is unique & the superclass exists *)
let populate_class (klass : stmt) (env : environment) : environment =
  match klass with
    ClassDecl(Object(n), Void, _) ->
      ensure_unique env n;
      define_class env n ""
  | ClassDecl(Object(n), Object(s), _) ->
      ensure_unique env n;
      if class_exists env s then (define_class env n s) else error "superclass not defined"
  | _ -> error "check_class_name must be called with a class"

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

let populate_variables (klass : stmt) (env : environment) : environment =
  let statement_filter = (fun x -> match x with Declare(_) -> true | _ -> false) in
  match klass with
    ClassDecl(Object(n), _, xs) ->
      let var_accu = (fun env x -> populate_variable x n env) in
      let statements = List.filter statement_filter xs in
      List.fold_left var_accu env statements
  | _ -> error "check_variables called against a non-class"


let populate_method (meth : stmt) class_name env : environment =
  let c = find_def env class_name in
  match meth with
    MethodDecl(e, xs) ->
      (match e.e_guts with
        Ident(s) ->
          ensure_unique c.d_env s;
          define_method c.d_env s;
          env
      | _ -> error "method name should be an identifier")
  | _ -> error "check_method called with a non-method stmt"
  env

let populate_methods (klass : stmt) env : environment =
  let method_filter = (fun x -> match x with MethodDecl(_) -> true | _ -> false) in
  match klass with
    ClassDecl(Object(n), _, xs) ->
      let meth_accu = (fun env x -> populate_method x n env) in
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
  let class_accu = (fun env x -> populate_class x env) in
  let var_accu = (fun env x -> populate_variables x env) in
  let meth_accu = (fun env x -> populate_methods x env) in
  let env' = List.fold_left class_accu env classes in
  let env'' = List.fold_left var_accu env' classes in
  List.fold_left meth_accu env'' classes

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
