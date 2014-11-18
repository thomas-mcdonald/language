open Dict
open Tree

let check_class (klass : stmt) =
  match klass with
    ClassDecl(e, xs) -> ()
  | _ -> raise (Failure "top level must be a class")

let check_classes (classes : stmt list) =
  List.map check_class classes;
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
