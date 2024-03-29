%{
open Dict
open Keiko
open Tree
%}

%token <int> NUMBER
%token <string> IDENT TYPE
%token CLASS COMMA DEF DOT EQUALS EOF END GT NEW LPAREN RPAREN SEMI
%token MINUS PLUS
/* TODO - should these be specialcased? */
%token BOOL COLON FALSE INT PUTS SWITCH SUPER THIS TRUE WHEN

%type <Tree.program> program
%start program

%{

let makeExpr x = { e_guts = x; e_type = Unknown }
let makeName x = { n_name = x; n_def = unknown_def }

let classObject = { n_name = "Object"; n_def = unknown_def }

%}

%%

program :
    classes { Prog ($1) }

classes:
    /* empty */   { [] }
  | classes klass { $1 @ [$2] }

klass:
    CLASS typename stmts END              { Klass($2, classObject, $3) }
  | CLASS typename GT typename stmts END  { Klass($2, $4, $5) }

stmts:
    /* empty */ { [] }
  | stmts stmt { $1 @ [$2] }

stmt:
    DEF identname opt_type stmts END                     { MethodDecl($2, [], $4, $3) }
  | DEF identname LPAREN argsd RPAREN opt_type stmts END { MethodDecl($2, $4, $7, $6) }
  | ident EQUALS expr                     { Assign($1, Unknown, $3) }
  | ident EQUALS LPAREN typed RPAREN expr { Assign($1, $4, $6) }
  | BOOL ident                     { Declare(Bool, $2) }
  | INT ident                      { Declare(Integer, $2) }
  | typed ident                    { Declare($1, $2) }
  | expr                           { Expr($1) }

opt_type:
  | /* empty */ { Unknown }
  | COLON BOOL  { Bool }
  | COLON INT   { Integer }
  | COLON typed { $2 }

argsd:
    /* empty*/        { [] }
  | argd              { [$1] }
  | argd COMMA argsd  { $1 :: $3 }

argd:
  | typed identname   { Arg($2, $1) }
  | BOOL identname    { Arg($2, Bool) }
  | INT identname     { Arg($2, Integer) }

expr:
  | expr DOT ident   { makeExpr (Call($1, $3, [])) }
  | expr DOT ident LPAREN args RPAREN { makeExpr (Call($1, $3, $5)) }
  | expr PLUS expr   { makeExpr (Binop(Plus, $1, $3)) }
  | expr MINUS expr  { makeExpr (Binop(Minus, $1, $3)) }
  | NEW typed        { makeExpr (New($2)) }
  | PUTS expr        { makeExpr (Puts($2)) }
  | SWITCH expr switch_body END { makeExpr(Switch($2, $3)) }
  | ident   { $1 }
  | number  { $1 }
  | FALSE   { makeExpr (Boolean(0)) }
  | TRUE    { makeExpr (Boolean(1)) }
  | THIS    { makeExpr This }
  | SUPER   { makeExpr Super }

args:
    /* empty */     { [] }
  | arg             { [$1] }
  | arg COMMA args  { $1 :: $3 }

arg: expr { $1 }

switch_body:
  | /* empty */     { [] }
  | switch_stmt switch_body { $1 :: $2 }

switch_stmt:
  | WHEN typed stmts { When($2, $3) }

ident:
  IDENT { makeExpr (Ident(makeName($1))) }

typed:
  TYPE { Object(makeName $1) }

identname:
  IDENT { makeName $1 }

typename:
  TYPE { makeName $1 }

number:
  NUMBER { makeExpr (Number($1)) }
