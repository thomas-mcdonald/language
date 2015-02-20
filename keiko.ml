type codelab = int
type kind = Int
type ident
type op = Plus | Minus
type symbol = string

type literal =
    INT of int32
  | HEX of int32
  | SYM of symbol

(* icode -- type of intermediate instructions *)
(* http://spivey.oriel.ox.ac.uk/corner/Design_overview_for_OBC *)
type icode =
  | CONST of int                (* Constant (value) *)
  | GLOBAL of symbol            (* Constant (symbol, offset) *)
  | LOCAL of int                (* Local address (offset) *)
  | REGVAR of int               (* Register (index) *)
  | LOADC                       (* Load char *)
  | LOADW                       (* Load word *)
  | STOREC                      (* Store char *)
  | STOREW                      (* Store word *)
  | ARG of int                  (* Pass argument (index) *)
  | SLINK                       (* Pass static link *)
  | PCALL of int                (* Call procedure (nparams) *)
  | RESULTW                     (* Procedure result *)
  | MONOP of op                 (* Perform unary operation (op) *)
  | BINOP of op                 (* Perform binary operation (op) *)
  | BOUND                       (* Array bound check *)
  | NCHECK                      (* Null pointer check *)
  | LABEL of codelab            (* Set code label *)
  | JUMP of codelab             (* Unconditional branch (dest) *)
  | JUMPC of op * codelab       (* Conditional branch (cond, dest) *)
  | JCASE of codelab list * codelab (* Jump table *)

  | PROC of symbol * int * literal
  | END
  | DEFINE of string
  | WORD of literal

  (* Extra instructions *)
  | RETURN
  | SEQ of icode list
  | NEWLINE

let string_of_icode (w : icode) : string =
  match w with
  | CONST(i) -> Printf.sprintf "CONST %d" i
  | GLOBAL(s) -> Printf.sprintf "GLOBAL %s" s
  | LOCAL(i) -> Printf.sprintf "LOCAL %d" i
  | LOADW -> "LOADW"
  | PCALL(i) -> Printf.sprintf "PCALL %d" i

  | PROC(s,f,_) -> Printf.sprintf "PROC %s %d 0 0" s f
  | END -> "END\n"
  | DEFINE(s) -> "DEFINE " ^ s
  | WORD(SYM(s)) -> "WORD " ^ s

  | RETURN -> "RETURN"
  | NEWLINE -> ""
