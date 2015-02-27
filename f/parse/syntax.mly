%{
open Ast
open Types
open ParseAux
%}

%token TYPE OF MATCH WITH AS AND LET REC FUNC IN IF THEN ELSE BEGIN END LISTCONS
%token LPAREN RPAREN LBRAC RBRAC SEMICOLON ARROW EOF COMMA EQUAL VERTBAR 
%token<string> LID UID TID OP0 OP1 OP2
%token<int> INTNUM
%token<float> FLOATNUM

%nonassoc SEMICOLON
%nonassoc IN
%nonassoc LET
%nonassoc WITH (* WITH < VERTBAR: match a with b -> match c with | d -> .. | e -> .. *)
%nonassoc THEN
%nonassoc ELSE
%left VERTBAR
%nonassoc AS
%right ARROW (* a -> b -> c *) 
%left COMMA
%left EQUAL OP0
%right LISTCONS
%left OP1
%left OP2
%nonassoc LPAREN LBRAC LID INTNUM FLOATNUM BEGIN UID

%start prog
%type <Ast.stmt list> prog
%type <stmt> stmt
%type <expr> expr, atom_expr
%type <string * (pattern list) * expr> letbinding
%type <(pattern * expr) list> match_clauses

%type <pattern> pattern

%type <(finalized, string) type_tag_b> type_sig, atomic_type_sig
%type <stmt> altype_decl
%type <string list> decl_param
%type <string * ((finalized, string) type_tag_b) option> altype_body

%%

%public tuple(X, Y, Z):
| x = X; y = Y; z = Z	{ (x, y, z) }

prog:	stmt+ EOF	{ $1 }
;

stmt:	expr SEMICOLON	{ Expr $1 }
|	altype_decl	{ $1 }
|	LET h = letbinding t = preceded(AND, letbinding)* SEMICOLON
	{ GLetExp (Let (NonRec, h :: t, IntConst 0)) }
|	LET REC h = letbinding t = preceded(AND, letbinding)* SEMICOLON
	{ GLetExp (Let (Rec, h :: t, IntConst 0)) }
;

%inline op:	EQUAL	{ "=" (* FIXME *) }
|	_op = OP0	{ _op }
|	_op = OP1	{ _op }
|	_op = OP2	{ _op }
;

expr:	atom_expr	{ $1 }
|	expr atom_expr
		{ match $1 with
		| AlType (s, []) when isUID s -> (
		  match $2 with
		  | Tuple lst -> AlType (s, lst) 
		  | _ -> AlType (s, [$2]))
		| _ -> FunApp ($1, $2) }
|	l = expr; o = op; r = expr
		{ FunApp (FunApp (Var o, l), r) }
|	l = expr; LISTCONS; r = expr
		{ AlType ("list", [l; r]) }
|	LET h = letbinding t = preceded(AND, letbinding)* IN b = expr
		{ Let (NonRec, h :: t, b) }
|	LET REC h = letbinding t = preceded(AND, letbinding)* IN b = expr
		{ Let (Rec, h :: t, b) }
|	FUNC atom_pattern+ ARROW expr
		{ Func ($2, $4) }
|	MATCH v = expr WITH VERTBAR? cl = match_clauses
		{ MatchExp (v, cl) }
|	IF expr THEN expr 
		{ IfExp ($2, $4, None) }
|	IF expr THEN expr ELSE expr (* to specify prec *)
		{ IfExp ($2, $4, Some $6) }
;

(* to specify prec *)
match_clauses:	pattern ARROW expr
		{ [($1, $3)] }
|	hd = match_clauses VERTBAR p = pattern ARROW e = expr
		{ hd @ [(p, e)] }
;

atom_expr:	LID	{ Var $1 }
|	INTNUM	{ IntConst $1 }
|	FLOATNUM	{ FltConst $1 }
|	LPAREN o = op RPAREN
		{ Var o }
|	UID	{ AlType ($1, []) }
|	LPAREN expr RPAREN
		{ $2 }
|	LPAREN; l = expr; r = preceded(COMMA, expr)+; RPAREN
		{ Tuple (l :: r) }
|	LBRAC; RBRAC
		{ AlType ("list_empty", []) }
|	LBRAC; l = expr; r = preceded(COMMA, expr)*; RBRAC
		{ ParseAux.listLiteralOf (l :: r) }
|	BEGIN; l = expr; r = preceded(SEMICOLON, expr)*; END
		{ Grouped (l :: r) }
;

letbinding:	LID atom_pattern* EQUAL expr
		{ ($1, $2, $4) }


(* --- PATTERN --- *)

pattern:
| atom_pattern	{ $1 }
| pattern AS LID
	{ PAlias ($3, $1) }
| pattern VERTBAR pattern
	{ POr ($1, $3) }
| pattern LISTCONS pattern
	{ PAlType ("list", [$1; $3]) }
| UID atom_pattern?
	{ match $2 with
	  | Some (PTuple lst) -> PAlType ($1, lst)
	  | Some rst -> PAlType ($1, [rst])
	  | None -> PAlType ($1, []) }
;

atom_pattern:
| INTNUM	{ PInt $1 }
| FLOATNUM	{ PFloat $1 }
| LID	{ PVar $1 }
| LPAREN pattern RPAREN
	{ $2 }
| LPAREN; hd = pattern; tl = preceded(COMMA, pattern)+; RPAREN
	{ PTuple (hd :: tl) }
| LBRAC RBRAC
	{ PAlType ("list_empty", []) }


(* --- TYPETAG --- *)

altype_decl:
  TYPE param_opt = decl_param?; id = LID;
  EQUAL VERTBAR?;
  hd = altype_body;
  tl = preceded(VERTBAR, altype_body)*;
  SEMICOLON
    { let param = match param_opt with None -> [] | Some t -> t in
      let pmap = List.mapi (fun i s -> (s, i)) param in
      let body = 
      List.map 
      (function
       | (s, Some t) -> (s, Some (updateSig pmap t))
       | (s, None) -> (s, None))
      (hd :: tl) 
      in
      AlTypeDecl(id, List.map snd pmap, body) }
;

decl_param:
  TID	{ [$1] }
| LPAREN hd = TID; tl = preceded(COMMA, TID)+ RPAREN
	{ hd :: tl }

altype_body:
  UID preceded(OF, type_sig)?
	{ match $2 with
	  | Some (TTuple _) -> ($1, $2)
	  | Some rst -> ($1, Some (TTuple [rst]))
	  | _ -> ($1, $2) }
;

type_sig:
  atomic_type_sig	
	{ $1 }
| type_sig ARROW type_sig 
	{ TFunc ($1, $3) }
| atomic_type_sig LID
	{ match $1 with
	| TVar a -> TAlType ($2, [$1])
	| TTuple l -> TAlType ($2, l)
	| _ -> failwith ""
	}
;

atomic_type_sig:
  TID	{ TVar $1 }
| LPAREN type_sig RPAREN 
	{ $2 }
| LPAREN type_sig preceded(COMMA, type_sig)+ RPAREN
	{ TTuple ($2 :: $3) }

(* vim: ts=16: noexpandtab: indentexpr=: ai
* *)
