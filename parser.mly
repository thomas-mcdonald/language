%{
open Tree
%}

%token <int> NUMBER
%token <string> IDENT
%token CLASS DEF EOF END SEMI

%type <Tree.program> program
%start program

%{
  
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
    CLASS ident stmts END { ClassDecl ($2, $3) }
  | DEF ident stmts END { MethodDecl($2, $3) }
  
ident:
  IDENT { Ident $1 }
