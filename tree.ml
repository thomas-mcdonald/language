type program = Prog of block

and block = Block of stmt list
          | NoBlock

and stmt = ClassDecl of expr * stmt list (* name * statements *)
         | MethodDecl of expr * stmt list (* name * statements  *)
         | Assign of expr * expr (* lhs = rhs *)
         | Declare of expr (* int x *)

and expr = Number of int
         | Const of string
         | Ident of string

and name = string

let ppExpr =
  function
    Number x -> "Number " ^ string_of_int x
  | Const x -> "Const " ^ x
  | Ident x -> "Ident " ^ x

let rec ppStmt =
  function
    ClassDecl (e, xs) -> "Class\nName - " ^ ppExpr e ^ "\nStmts - \n" ^ ppStmts xs
  | MethodDecl (e, xs) -> "Method\nName - " ^ ppExpr e ^ "\nStmts - \n" ^ ppStmts xs
  | Assign (e, e') -> "Assignment\n" ^ ppExpr e ^ "\n" ^ ppExpr e'
  | Declare(e) -> "Variable declaration " ^ ppExpr e

and ppStmts xs = String.concat "\n" (List.map ppStmt xs)

let ppBlock =
  function
    NoBlock -> "NoBlock"
  | Block xs -> String.concat "\n" (List.map ppStmt xs)

let ppProg =
  function
    Prog b -> ppBlock b
