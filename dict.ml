module EnvMap = Map.Make(String)

type environment = Env of def EnvMap.t ref

and def_type = ClassDef of string (* class (superclass) *)
             | VarDef (* variable *)

and def = {
  (* d_tag : int; (* unique id *) *)
  d_name : string; (* name of element *)
  d_type : def_type;
  d_env : environment; (* environment in this scope *)
  }

let new_env : environment = Env (ref EnvMap.empty)

let find_def env x =
  match env with
    Env(m) -> EnvMap.find x !m
  | _ -> raise Not_found

let def_exists env x =
  try
    find_def env x; true
  with
    Not_found -> false

let define_class env name supername =
  let d = { d_name = name; d_type = ClassDef(supername); d_env = new_env } in
  match env with
    Env(m) -> m := EnvMap.add name d !m; env

let class_exists env name =
  try
    let d = find_def env name in
    match d.d_type with
      ClassDef(_) -> true
    | _ -> false
  with
    Not_found -> false