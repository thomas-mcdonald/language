open Dict

type ident = string

type name =
  { n_name: ident;
    mutable n_def: def }

type typed = Integer | Boolean | Object of name | Unknown

type program = Prog of klass list

and klass = Klass of name * name * stmt list

and block = Block of stmt list
          | NoBlock

and stmt = MethodDecl of name * method_arg list * stmt list (* name * args * statements  *)
         | Assign of expr * expr (* lhs = rhs *)
         | Declare of typed * expr (* int x *)
         | Expr of expr (* expression statement  *)

and method_arg = Arg of name * typed

and expr =
  { e_guts: expr_guts;
    mutable e_type: typed }

and expr_guts = Number of int
         | Const of string
         | Ident of string
         | Boolean of int
         | Binop of op * expr * expr
         | Call of expr * expr * expr list (* object.method(args) *)
         | Nil

and op = Plus
