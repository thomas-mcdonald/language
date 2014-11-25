open Dict
open Tree

let error (str:string) =
  Printf.eprintf "%s" str;
  exit 1

let check_class_name (klass : stmt) (classes : string list) : string =
  let name_check = (fun x -> if List.mem x classes then
    error ("class " ^ x ^ " already defined")
  else x) in
  match klass with
    ClassDecl(Object(n), Void, _) ->
      name_check n
  | ClassDecl(Object(n), Object(s), _) ->
      let name = name_check n in
      if List.mem s classes then name else error "superclass not defined"
  | _ -> error "check_class_name must be called with a class"

(*
  check_classes extracts information about the classes in the program.
  This is done in multiple passes.
    * Fold through the list of classes, gathering a list of class names and verifying
      * no duplicate names
      * superclasses are already defined
    * Extract list of class names
    * Check superclasses exist
    * Generate list of instance variables & check their types
    * Generate list of methods
*)
let check_classes (classes : stmt list) =
  let accu = (fun acc x -> (check_class_name x acc)::acc) in
  let class_names = List.fold_left accu [] classes in
  ()

let check_block (env : environment) (block : block) =
  match block with
      NoBlock -> raise (Failure "no program given")
    | Block(xs) -> check_classes xs

let annotate (program : program) : unit =
  let env = Env (ref []) in
  match program with
    Prog(block) ->
      check_block env block;
      ()
