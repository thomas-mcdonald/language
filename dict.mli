type environment


and def_type = ClassDef of string (* class*)
             | VarDef (* variable *)

and def = {
  d_name : string; (* name of element *)
  d_type : def_type;
  d_env : environment (* environment in this scope *)
  }

(* Create a new environment *)
val new_env : environment

(* Find a definition in the top level of a given environment *)
val find_def : environment -> string -> def

(* Does the given definition exist *)
val def_exists : environment -> string -> bool

(* class specific environment methods *)
(* define a class (name, superclass) *)
val define_class : environment -> string -> string -> environment

(* does a class exist with the name? used for finding superclasses *)
val class_exists : environment -> string -> bool
