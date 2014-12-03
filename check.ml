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
  let accu = (fun env x -> check_class x env) in
  List.fold_left accu env classes

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
