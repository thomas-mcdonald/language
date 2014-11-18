type environment = Env of (def list ref)

and def_type = ClassDef (* class*)
             | VarDef (* variable *)

and def = {
  d_tag : int; (* unique id *)
  d_type: def_type;
  }
