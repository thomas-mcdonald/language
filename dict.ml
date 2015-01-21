module EnvMap = Map.Make(String)

type environment = Env of def EnvMap.t ref

and def_type = ClassDef of class_data (* class data *)
             | VarDef of type_data (* variable (class type) *)
             | MethDef of meth_data (* method *)
             | UnknownDef

and def = {
  (* d_tag : int; (* unique id *) *)
  d_name : string; (* name of element *)
  mutable d_type : def_type; (* type, discovered in sem stage *)
  d_env : environment; (* environment in this scope *)
  }

and class_data = {
  c_depth : int; (* how many classes above? *)
  mutable c_methods : def list; (* method list *)
  c_super : def option
}

and meth_data = {
  m_receiver : def;
}

and type_data = Int | Object of def ref

let new_env = fun () -> Env (ref EnvMap.empty)

(* pull class_data out of a def *)
let find_class_data (d : def) =
  match d.d_type with ClassDef(c) -> c | _ -> failwith "find_class_data"

(* return a new class data struct. arg is for super *)
let new_class_data x =
  match x with
    Some(d) ->
      let parent_cd = find_class_data d in
      { c_depth = parent_cd.c_depth + 1; c_methods = []; c_super = x; }
  | None ->     { c_depth = 0; c_methods = []; c_super = x; }

let add_def env n d =
  match env with
    Env(m) -> m := EnvMap.add n d !m; env

(* the initial environment. contains the object godclass *)
let initial_env : environment =
  let env = new_env () in
  let d = { d_name = "Object"; d_type = ClassDef(new_class_data None); d_env = new_env () } in
  add_def env "Object" d

let find_def env x =
  match env with
    Env(m) -> EnvMap.find x !m

let def_exists env x =
  try
    find_def env x; true
  with
    Not_found -> false


let unknown_def = { d_name = "unknown"; d_type = UnknownDef; d_env = new_env () }

(* class methods *)
let define_class env (name : string) (super : string) =
  let sc = find_def env super in
  let d = { d_name = name; d_type = ClassDef(new_class_data (Some sc)); d_env = new_env () } in
  add_def env name d

let class_exists env name =
  try
    let d = find_def env name in
    match d.d_type with
      ClassDef(_) -> true
    | _ -> false
  with
    Not_found -> false

(* variable methods *)
let define_variable env name c =
  let d = { d_name = name; d_type = VarDef(c); d_env = new_env () } in
  add_def env name d

