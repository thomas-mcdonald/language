type ptype = Integer | Boolean | Object | Void

type program = Prog of block

and block = Block of stmt list
          | NoBlock

and stmt = ClassDecl of expr * stmt list (* name * statements *)
         | MethodDecl of expr * stmt list (* name * statements  *)
         | Assign of expr * expr (* lhs = rhs *)
         | Declare of expr (* int x *)

and expr =
  { e_guts: expr_guts;
    mutable e_type: ptype }

and expr_guts = Number of int
         | Const of string
         | Ident of string
         | Binop of op * expr * expr
         | Call of expr * expr * expr list (* object.method(args) *)

and op = Plus
and name = string

let makeExpr g =
  { e_guts = g; e_type = Void }
