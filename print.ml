(* print.ml - printing functions for the ast *)

open Tree

let nest xs =
  let newline = Str.regexp "\n" in
  let lines = Str.split newline xs in
  let comb = (fun acc x -> if acc == "" then x else acc ^ "\n  " ^ x) in
  List.fold_left comb "" lines

let wrap_simple (s : string) = "(" ^ s ^ ")"

let wraplist (id : string) (xs : string list) : string =
  wrap_simple (id ^ (String.concat "" (List.map nest xs)))

let ppType =
  function
    Integer -> "Integer"
  | Boolean -> "Boolean"
  | Object(s) -> s ^ " (Object)"
  | Void -> "Void"

let ppName n = n.n_name
let ppArgs args = wraplist "arguments" (List.map (fun a -> match a with Arg(n, p) -> n.n_name ^ " " ^ ppType p) args)

let rec ppExpr e =
  match e.e_guts with
    Number x -> wrap_simple ("Number " ^ string_of_int x)
  | Const x -> wrap_simple ("Const " ^ x)
  | Ident x -> wrap_simple ("Ident " ^ x)
  | Binop (Plus, e, e') -> wraplist "Add" [ppExpr e; ppExpr e']
  | Call (o, m, xs) -> wraplist "Call" [ppExpr o; ppExpr m] (* ^ nest (List.map ppExpr xs) *)
  | Nil -> "nil"

let rec ppStmt =
  function
  | MethodDecl (n, args, xs) -> wraplist "Method" [ppName n; ppArgs args; ppStmts xs]
  | Assign (e, e') -> wraplist "Assignment" [ppExpr e; ppExpr e']
  | Declare(p, e) -> wraplist ("Declaration " ^ (ppType p)) [ppExpr e]
  | Expr(e) -> wraplist "Expression" [ppExpr e]

and ppStmts xs = "[" ^ String.concat ",\n" (List.map ppStmt xs) ^ "]"

let ppClass =
  function
    Klass(n,s,xs) ->
      wraplist (Printf.sprintf "Class %s < %s" (ppName n) (ppName s)) [ppStmts xs]

let program =
  function
    Prog cs -> String.concat "\n" (List.map ppClass cs)
