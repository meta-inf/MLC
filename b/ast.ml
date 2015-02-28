type expr =
    IntConst of int
  | FltConst of float
  | BoolConst of bool
  | StrConst of string
  | Identifier of int
  | Cond of (expr * expr) list
  | Lambda of (int list) * expr
  | FunApp of expr * (expr list) (* general functions *)
  | FunApp1 of int * expr (* general functions *)
  | FunApp2 of int * expr * expr (* bulitin binary operations *)
  | Let of (int * expr) list * expr
  | LetRec of (int * expr) list * expr
  | Tuple of (expr list)
;;

