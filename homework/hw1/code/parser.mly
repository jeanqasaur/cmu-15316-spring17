%{
  open Ast

  let principal_of_id s =
    if s = "admin" then Admin
    else
      if s = "anyone" then Anyone
      else Principal s
%}

%token <string> STR ID 
%token AS PRINCIPAL RETURN DO TO EXIT FOREACH CHANGE PASSWORD
%token CREATE IN REPLACEWITH APPEND SET READ WRITE
%token EMPTYLIST EQUALS ARROW LBRACE RBRACE COMMA DOT
%token DELETE ALL LPAREN RPAREN
%token EQUAL NOTEQUAL FILTEREACH WITH
%token IF THEN ELSE
%token TRUE FALSE
%token EOL EOP

%start prog

%type <Ast.prog> prog
%type <Ast.expr> expr
%type <Ast.cmd list> cmd
%type <Ast.cmd> primcmd
%type <Ast.small_expr> value
%type <(Ast.ident * Ast.small_expr) list> fieldvals
%type <Ast.principal> principal
%type <Ast.small_expr list> args
%type <Ast.bool_expr> boolexpr

%%
prog:
  AS PRINCIPAL principal PASSWORD STR DO EOL cmd EOP { ($3,$5,$8) }
;

cmd:
| primcmd EOL cmd { $1::$3 }
| RETURN expr EOL { [Return $2] }
| EXIT EOL { [Exit] }
;

primcmd:
| CHANGE PASSWORD principal STR { Change_pass ($3, $4) }
| CREATE PRINCIPAL principal STR { Create_principal ($3, $4) }
| SET ID EQUALS expr { Set ($2,$4) }
| APPEND TO ID WITH expr { Append ($3,$5) }
| FILTEREACH ID IN ID WITH boolexpr { Filtereach ($2,$4,$6) }
;

expr:
| value { Val_expr $1 }
| EMPTYLIST { Empty_list }
| LBRACE fieldvals RBRACE { Rec_exp $2 }
| IF boolexpr THEN expr ELSE expr { If($2,$4,$6) }
;

value:
| ID { Var $1 }
| ID DOT ID { Field ($1,$3) }
| STR { String_exp $1 }
;

fieldvals:
| ID EQUALS value { [($1, $3)] }
| ID EQUALS value COMMA fieldvals { ($1,$3)::$5 }
;

principal:
| ID { principal_of_id $1 }
;

args:
| value { [$1] }
| value COMMA args { $1::$3 }
;

boolexpr:
| TRUE { True }
| FALSE { False }
| EQUAL LPAREN args RPAREN { Equal($3) }
| NOTEQUAL LPAREN args RPAREN { Notequal($3) }
;
