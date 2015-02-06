%{
open Dict
open Tree
%}

%token <int> NUMBER
%token <string> IDENT TYPE
%token CLASS COMMA DEF DOT EQUALS EOF END GT LPAREN PLUS RPAREN SEMI
/* todo - should these be specialcased? */
%token FALSE INT TRUE

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
    DEF identname stmts END                     { MethodDecl($2, [], $3) }
  | DEF identname LPAREN args RPAREN stmts END  { MethodDecl($2, $4, $6) }
  | ident EQUALS expr              { Assign($1, $3) }
  | INT ident                      { Declare(Integer, $2) }
  | typed ident                    { Declare($1, $2) }
  | expr                           { Expr($1) }

args:
    /* empty*/      { [] }
  | arg             { [$1] }
  | arg COMMA args  { $1 :: $3 }

arg:
  typed identname   { Arg($2, $1) }

expr:
    ident { $1 }
  | number { $1 }
  | FALSE { makeExpr (Boolean(2)) }
  | TRUE  { makeExpr (Boolean(1)) }
  | expr PLUS expr  { makeExpr (Binop(Plus, $1, $3)) }
  | expr DOT expr   { makeExpr (Call($1, $3, [])) }

ident:
  IDENT { makeExpr (Ident($1)) }

typed:
  TYPE { Object(makeName $1) }

identname:
  IDENT { makeName $1 }

typename:
  TYPE { makeName $1 }

number:
  NUMBER { makeExpr (Number($1)) }