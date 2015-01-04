%{
open Dict
open Tree
%}

%token <int> NUMBER
%token <string> IDENT TYPE
%token CLASS COMMA DEF DOT EQUALS EOF END GT LPAREN PLUS RPAREN SEMI
/* todo - should these be specialcased? */
%token INT

%type <Tree.program> program
%start program

%{

let makeName x = { n_name = x; n_def = unknown_def }

%}

%%

program :
    stmts { Prog (Block $1) }
  | SEMI  { Prog (NoBlock) } ;

/* stmts is a reversed list. */
stmts:
  reversed_stmts { List.rev $1 }

reversed_stmts:
    /* empty */ { [] }
  | stmts stmt { $2 :: $1 } /* remember to reverse this */

stmt:
    CLASS typename stmts END          { ClassDecl($2, Void, $3) }
  | CLASS typename GT ptype stmts END { ClassDecl($2, $4, $5) }
  | DEF identname stmts END                     { MethodDecl($2, [], $3) }
  | DEF identname LPAREN args RPAREN stmts END  { MethodDecl($2, $4, $6) }
  | ident EQUALS expr              { Assign($1, $3) }
  | INT ident                      { Declare(Integer, $2) }
  | ptype ident                    { Declare($1, $2) }
  | expr                           { Expr($1) }

args:
    /* empty*/      { [] }
  | arg             { [$1] }
  | arg COMMA args  { $1 :: $3 }

arg:
  ptype identname   { Arg($2, $1) }

expr:
    ident { $1 }
  | number { $1 }
  | expr PLUS expr  { makeExpr (Binop(Plus, $1, $3)) }
  | expr DOT expr   { makeExpr (Call($1, $3, [])) }

ident:
  IDENT { makeExpr (Ident($1)) }

ptype:
  TYPE { Object $1 }

identname:
  IDENT { makeName $1 }

typename:
  TYPE { makeName $1 }

number:
  NUMBER { makeExpr (Number($1)) }