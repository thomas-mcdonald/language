module EnvMap = Map.Make(String)

type environment = Env of def EnvMap.t ref

and def_type = ClassDef of def option ref * class_data (* class (superclass) * other class data *)
             | VarDef of def ref (* variable (class type) *)
             | MethDef (* method *)
             | UnknownDef

and def = {
  (* d_tag : int; (* unique id *) *)
  d_name : string; (* name of element *)
  mutable d_type : def_type; (* type, discovered in sem stage *)
  d_env : environment; (* environment in this scope *)
  }

and class_data = {
  mutable c_methods : def list (* method list *)
}

let new_env = fun () -> Env (ref EnvMap.empty)

let new_class_data = fun () -> { c_methods = [] }

let add_def env n d =
  match env with
    Env(m) -> m := EnvMap.add n d !m; env

(* the initial environment. contains the object godclass *)
let initial_env : environment =
  let env = new_env () in
  let d = { d_name = "Object"; d_type = ClassDef(ref None, new_class_data ()); d_env = new_env () } in
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
let define_class env name supername =
  let sc = if supername = "" then find_def env "Object" else find_def env supername in
  let d = { d_name = name; d_type = ClassDef(ref (Some sc), new_class_data ()); d_env = new_env () } in
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
  let d = { d_name = name; d_type = VarDef(ref c); d_env = new_env () } in
  add_def env name d

