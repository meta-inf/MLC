type pattern =
    PInt of int
  | PFloat of float
  | PVar of string
  | PTuple of pattern list
  | PAlType of string * (pattern list)
  | POr of pattern * pattern
  | PAlias of string * pattern

type let_label = NonRec | Rec

type expr = 
    IntConst of int
  | FltConst of float
  | StrConst of string
  | Var of string
  | Tuple of expr list
  | AlType of string * (expr list)
  | FunApp of expr * expr
  | Let of let_label * (string * (pattern list) * expr) list * expr
  | Grouped of expr list
  | Func of (pattern list) * expr
  | IfExp of expr * expr * (expr option)
  | MatchExp of expr * ((pattern * expr) list)

type stmt =
    Expr of expr
  | AlTypeDecl of string * (int list) * 
                  ((string * (Types.finalized Types.type_tag option)) list)
  | GLetExp of expr
