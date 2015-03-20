type environment

and def_type = ClassDef of class_data (* class data *)
             | MethDef of meth_data (* method *)
             | VarDef of var_data (* variable (class type) *)
             | UnknownDef

and def = {
  d_name : string; (* name of element *)
  mutable d_type : def_type;
  d_env : environment (* environment in this scope *)
  }

and class_data = {
  c_depth : int;
  mutable c_methods : def list; (* method list *)
  c_super : def option; (* superclass *)
  mutable c_size : int;
  mutable c_variables : def list
}

and meth_data = {
  m_receiver : def;
  m_return : type_data;
  mutable m_size : int;
  mutable m_offset : int;
  mutable m_arg_count : int;
  mutable m_args : def list;
}

and var_data = {
  v_offset : int;
  v_type : type_data;
  v_place : var_place;
}

and type_data = Bool | Int | Object of def ref | Void
(* is the variable on the class or the method? *)
and var_place = ClassVar | MethodVar | FunctionArg

exception Not_method

(* The initial environment *)
val initial_env : environment

val new_env : unit -> environment

(* pull class_data out of a def *)
val find_class_data : def -> class_data

(* add the definition to the environment *)
val add_def : environment -> string -> def -> environment

val remove_def : environment -> string -> environment

val find_meth_data : def -> meth_data

val find_var_data : def -> var_data

(* Find a definition in the top level of a given environment *)
val find_def : environment -> string -> def

(* Does the given definition exist *)
val def_exists : environment -> string -> bool

(* placeholder def for use in name *)
val unknown_def : def

(* class specific environment methods *)
(* define a class (name, superclass) *)
val define_class : environment -> string -> string -> environment

(* does a class exist with the name? used for finding superclasses *)
val class_exists : environment -> string -> bool
