open Dict
open Tree

let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let ensure_unique env x =
  if def_exists env x then error (x ^ " is already defined")

(* check_class checks that the class name is unique & the superclass exists *)
let check_class (klass : stmt) (env : environment) : environment =
  match klass with
    ClassDecl(Object(n), Void, _) ->
      ensure_unique env n;
      define_class env n ""
  | ClassDecl(Object(n), Object(s), _) ->
      ensure_unique env n;
      if class_exists env s then (define_class env n s) else error "superclass not defined"
  | _ -> error "check_class_name must be called with a class"

let check_variable (dec : stmt) class_name env : environment =
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

let check_variables (klass : stmt) (env : environment) : environment =
  let statement_filter = (fun x -> match x with Declare(_) -> true | _ -> false) in
  match klass with
    ClassDecl(Object(n), _, xs) ->
      let var_accu = (fun env x -> check_variable x n env) in
      let statements = List.filter statement_filter xs in
      List.fold_left var_accu env statements
    | _ -> error "check_variables called against a non-class"


(*
  check_classes extracts information about the classes in the program.
  This is done in multiple passes.
    * Fold through the list of classes, gathering a list of class names and verifying
      * no duplicate names
      * superclasses are already defined
    * Generate list of instance variables & check their types
    * Generate list of methods
*)
let check_classes (classes : stmt list) (env : environment) =
  let class_accu = (fun env x -> check_class x env) in
  let var_accu = (fun env x -> check_variables x env) in
  let env' = List.fold_left class_accu env classes in
  List.fold_left var_accu env' classes

let check_block (env : environment) (block : block) =
  match block with
      NoBlock -> raise (Failure "no program given")
    | Block(xs) -> check_classes xs env

let annotate (program : program) : unit =
  let env = initial_env in
  match program with
    Prog(block) ->
      check_block env block;
      ()
