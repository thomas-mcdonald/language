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

let rec ppExpr e =
  match e.e_guts with
    Number x -> wrap_simple ("Number " ^ string_of_int x)
  | Const x -> wrap_simple ("Const " ^ x)
  | Ident x -> wrap_simple ("Ident " ^ x)
  | Binop (Plus, e, e') -> wraplist "Add" [ppExpr e; ppExpr e'] (* ^ nest (ppExpr e) ^ nest (ppExpr e') *)
  | Call (o, m, xs) -> wraplist "Call" [ppExpr o; ppExpr m] (* ^ nest (List.map ppExpr xs) *)
  | Nil -> "nil"

let rec ppStmt =
  function
    ClassDecl (e, e', xs) -> wraplist "Class" [ppExpr e; ppExpr e'; ppStmts xs]
  | MethodDecl (e, xs) -> wraplist "Method" [ppExpr e; ppStmts xs]
  | Assign (e, e') -> wraplist "Assignment" [ppExpr e; ppExpr e']
  | Declare(e) -> wraplist "Declaration " [ppExpr e]

and ppStmts xs = "[" ^ String.concat ",\n" (List.map ppStmt xs) ^ "]"

let ppBlock =
  function
    NoBlock -> "NoBlock"
  | Block xs -> String.concat "\n" (List.map ppStmt xs)

let program =
  function
    Prog b -> ppBlock b ^ "\n"
