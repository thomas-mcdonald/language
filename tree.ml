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

let nest xs =
  let newline = Str.regexp "\n" in
  let lines = Str.split newline xs in
  let comb = (fun acc x -> if acc == "" then x else acc ^ "\n  " ^ x) in
  List.fold_left comb "" lines

let rec ppExpr e =
  match e.e_guts with
    Number x -> "Number " ^ string_of_int x
  | Const x -> "Const " ^ x
  | Ident x -> "Ident " ^ x
  | Binop (Plus, e, e') -> "Add" ^ nest (ppExpr e) ^ nest (ppExpr e')
  | Call (o, m, xs) -> "Call" ^ nest (ppExpr o) ^ nest (ppExpr m) (* ^ nest (List.map ppExpr xs) *)

let rec ppStmt =
  function
    ClassDecl (e, xs) -> "Class" ^ nest (ppExpr e) ^ nest (ppStmts xs)
  | MethodDecl (e, xs) -> "Method" ^ nest (ppExpr e) ^ nest (ppStmts xs)
  | Assign (e, e') -> "Assignment" ^ nest (ppExpr e) ^ nest (ppExpr e')
  | Declare(e) -> "Declaration " ^ nest (ppExpr e)

and ppStmts xs = String.concat "\n" (List.map ppStmt xs)

let ppBlock =
  function
    NoBlock -> "NoBlock"
  | Block xs -> String.concat "\n" (List.map ppStmt xs)

let ppProg =
  function
    Prog b -> ppBlock b
