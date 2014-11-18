type ptype = Integer | Boolean | Object | Void

type program = Prog of block

and block = Block of stmt list
          | NoBlock

and stmt = ClassDecl of expr * expr * stmt list (* name * superclass * statements *)
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
         | Nil

and op = Plus
and name = string

let makeExpr g =
  { e_guts = g; e_type = Void }

let classMatch =
  function
    ClassDecl(_,_,_)  -> true
  | _               -> false

let extract_classes (stmts : stmt list) : stmt list =
  List.filter classMatch stmts
