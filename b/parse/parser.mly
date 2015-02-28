%{
%}

%token <float> FLOATNUM
%token <int> INTNUM
%token <bool> BOOLNUM
%token <string> STRCONST
%token <int> IDENTIFIER
%token LBRACKET RBRACKET TUPLE LET LETREC COND LAMBDA 
%token NEWLINE

%start expr
%type <Ast.expr> expr
%type <Ast.expr list> tuplelist
%type <(Ast.expr * Ast.expr) list> condlist
%type <Ast.expr> funAppList
%type <(int * Ast.expr) list> letbindings
%type <(int * Ast.expr) list> letrecbindings

%%

expr:	  INTNUM		{ Ast.IntConst $1 }
	| FLOATNUM		{ Ast.FltConst $1 }
	| BOOLNUM		{ Ast.BoolConst $1 }
	| STRCONST		{ Ast.StrConst $1 }
	| IDENTIFIER		{ Ast.Identifier $1 }
	| funAppList RBRACKET	{ $1 }
	| tuplelist RBRACKET	{ Ast.Tuple $1 }
	| condlist RBRACKET	{ Ast.Cond $1 }
	| LBRACKET letbindings RBRACKET expr RBRACKET { Ast.Let ($2, $4) }
	| LBRACKET letrecbindings RBRACKET expr RBRACKET { Ast.LetRec ($2, $4) }
	| lambdaHead RBRACKET expr RBRACKET { Ast.Lambda ($1, $3) }
;

lambdaHead:	  LBRACKET LAMBDA LBRACKET IDENTIFIER { [$4] }
	| lambdaHead IDENTIFIER	{ $1 @ [$2] }

funAppList:       LBRACKET expr expr	{ Ast.FunApp ($2, [$3]) }
	| funAppList expr	{ Ast.FunApp ($1, [$2]) }
;

tuplelist:	  LBRACKET TUPLE		{ [] }
	| tuplelist expr	{ List.append $1 [$2] }
;

condlist:	  LBRACKET COND		{ [] }
                | condlist LBRACKET expr expr RBRACKET	{ $1 @ [($3, $4)] }
;

letbindings:	  LET LBRACKET		{ [] }
                | letbindings LBRACKET IDENTIFIER expr RBRACKET { $1 @ [($3, $4)] }
;

letrecbindings:	  LETREC LBRACKET		{ [] }
                | letrecbindings LBRACKET IDENTIFIER expr RBRACKET { $1 @ [($3, $4)] }
;

%%

(* vim: ts=16: noexpandtab
* *)
