open Ast
open Types

exception Parse_error of string

let rec updateSig 
  : (string * int) list -> ((finalized, string) type_tag_b) -> finalized type_tag
    = fun pmap ty ->
      try
        match ty with
        | TInt | TFloat | TBool | TUnit as ret -> ret
        | TVar s -> TVar (List.assoc s pmap)
        | TFunc (a, b) -> TFunc (updateSig pmap a, updateSig pmap b)
        | TTuple lst -> TTuple (List.map (updateSig pmap) lst)
        | TAlType (s, lst) -> 
          TAlType(s, List.map (updateSig pmap) lst)
      with Not_found -> raise (Parse_error "unbounded type variable")

let listLiteralOf lst = 
  List.fold_right 
    (fun cur exp -> AlType ("list", [cur; exp])) 
    lst (AlType ("list_empty", []))

let isUID s = 
  match s.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false
