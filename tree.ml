type program = Prog of block

and block = Block of stmt list
          | NoBlock

and stmt = ClassDecl of expr * stmt list (* name * statements *)
         | MethodDecl of expr * stmt list (* name * statements  *)
         | Assign of expr * expr (* lhs = rhs *)

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
    ClassDecl (e, xs) -> "Class\n" ^ "Name - " ^ ppExpr e ^ "\nStmts - \n" ^ ppStmts xs
  | MethodDecl (e, xs) -> "Method\n" ^ "Name - " ^ ppExpr e ^ "\nStmts - \n" ^ ppStmts xs

and ppStmts xs = String.concat "\n" (List.map ppStmt xs)

let ppBlock =
  function
    NoBlock -> "NoBlock"
  | Block xs -> String.concat "\n" (List.map ppStmt xs)

let ppProg =
  function
    Prog b -> ppBlock b
