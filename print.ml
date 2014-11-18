(* print.ml - printing functions for the ast *)

open Tree

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
  | Nil -> "nil"

let rec ppStmt =
  function
    ClassDecl (e, e', xs) -> "Class" ^ nest (ppExpr e) ^ nest (ppExpr e') ^ nest (ppStmts xs)
  | MethodDecl (e, xs) -> "Method" ^ nest (ppExpr e) ^ nest (ppStmts xs)
  | Assign (e, e') -> "Assignment" ^ nest (ppExpr e) ^ nest (ppExpr e')
  | Declare(e) -> "Declaration " ^ nest (ppExpr e)

and ppStmts xs = String.concat "\n" (List.map ppStmt xs)

let ppBlock =
  function
    NoBlock -> "NoBlock"
  | Block xs -> String.concat "\n" (List.map ppStmt xs)

let program =
  function
    Prog b -> ppBlock b ^ "\n"
