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
    PUSH of int32		(* Push constant (value) *)
  | LOCAL of int		(* Push address (offset) *)
  | LOAD of int			(* Load (size) *)
  | STORE of int		(* Store (size) *)
  | CONST of int * int		(* Load constant (offset, size) *)
  | FIXCOPY			(* Copy multiple values *)
  | FLEXCOPY			(* Copy open array param *)
  | DUP of int			(* Duplicate n'th value on stack (n) *)
  | POP of int			(* Pop a value (size) *)
  | SWAP			(* Swap top two values on stack *)
  | CALL of int * int           (* Call proc (nparams, res size) *)
  | RETURN 			(* Return from procedure *)
  | MONOP of kind * op		(* Unary operation (type, op) *)
  | BINOP of kind * op		(* Binary operation *)
  | CONV of kind * kind		(* Type conversion *)
  | ALIGN of int		(* Align parameter (size) *)
  | BOUND of int		(* Array bound check (line) *)
  | NCHECK of int		(* Check for null pointer (line) *)
  | ZCHECK of kind * int	(* Check for zero divisor (line) *)
  | ERROR of symbol * int	(* Runtime error (kind, line) *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPB of bool * codelab	(* Jump on boolean *)
  | JUMPC of kind * op * codelab  (* Cond. branch (type, cond, dest) *)
  | JCASE of codelab list       (* Case jump *)
  | JRANGE of codelab		(* Range jump *)
  | TYPETEST of int		(* Type test (level) *)

  | PROC of symbol * int * literal  (* Procedure (label, frame, gcmap) *)
  | PRIMDEF of symbol * string * int * literal
				(* Primitive (label, prim, frame, gcmap) *)
  | END 			(* End of proc: shouldn't reach here *)
  | LABEL of codelab		(* Set code label *)
  | DEFINE of symbol		(* Label for record descriptor *)
  | STRING of string		(* String in data space (text) *)
  | GLOBAL of symbol * int	(* Global variable *)
  | WORD of literal		(* Data word (value) *)
  | FLOAT of float		(* Float constant (value) *)
  | DOUBLE of float		(* Double constant (value) *)
  | MODULE of ident * int * int	(* Module header (name, stamp, line count) *)
  | IMPORT of ident * int	(* Module import (name, stamp) *)
  | PRIM of string		(* Declare primitive *)
  | ENDHDR			(* End of definitions *)
  | STKMAP of literal		(* Stack map *)
  | LINE of int			(* Line number *)
  | COMMENT of string		(* Comment *)
  | BLANK			(* Blank line *)

  | INDEX of int		(* PUSH s/BINOP Times/BINOP PlusA *)
  | LDLW of int		(* LDWL n *)
  | STL of int * int		(* LOCAL n/STORE s *)
  | LDG of int * int		(* GETK n/LOAD s *)
  | STG of int * int		(* GETK n/STORE s *)
  | LDI of int			(* INDEX s/LOAD s *)
  | STI of int			(* INDEX s/STORE s *)
  | LDNW of int			(* PUSH n/LDI 4 *)
  | STNW of int			(* PUSH n/STI 4 *)
  | LDEW of int			(* LDLW 12/LDNW n *)
  | STEW of int			(* LDLW 12/STNW n *)
  | JUMPCZ of kind * op * codelab  (* PUSH 0/JUMPC *)
  | TESTGEQ of codelab		(* Case split = DUP 1/JUMPC Lt *)

  | SEQ of icode list
  | NEWLINE

let string_of_icode (w : icode) : string =
  match w with
    PROC(s,f,_) -> Printf.sprintf "PROC %s %d 0 0" s f
  | END -> "END\n"
  | DEFINE(s) -> "DEFINE " ^ s
  | WORD(SYM(s)) -> "WORD " ^ s

  | LDLW(i) -> "LDLW " ^ (string_of_int i)

  | NEWLINE -> ""
