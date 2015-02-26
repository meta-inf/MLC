(* reduce0.ml: flatten calls to binary builtin functions 
 * *)

open Ast
open Value

module IntSet = Set.Make(struct type t = int let compare = compare end)

let builtinFuncID = 
  let f = List.fold_left (fun tb id -> IntSet.add id tb) IntSet.empty 
  and g = List.map (function (x, y) -> Parse.IdTable.find x)
  in f (g builtinFuncList)

(* due to a bug in type inferer *)
let builtinFunc =
  let f = List.map (function (x, y) -> (Parse.IdTable.find x, y)) in
  let g lst = Array.init 
      ((List.fold_left max 0 (IntSet.elements builtinFuncID)) + 1)
      (fun id -> try List.assoc id (f lst) with Not_found -> Unit) in
  let arr1 = g builtinFuncList in fun id -> arr1.(id)

let binOpFunc = 
  let f = List.map (function (x, y) -> (Parse.IdTable.find x, y)) in
  let g lst = Array.init 
      ((List.fold_left max 0 (IntSet.elements builtinFuncID)) + 1)
      (fun id -> 
         try List.assoc id (f lst) 
         with Not_found -> snd @@ List.hd lst) in
  let arr0 = g builtinBinOps in fun id -> arr0.(id)

let rec reduce0 expr env =
  match expr with
  | FunApp (FunApp (Identifier id, v0), v1)
    when (IntSet.mem id env) -> FunApp2 (id, reduce0 v0 env, reduce0 v1 env)
  | FunApp (Identifier id, v)
    when (IntSet.mem id env) -> FunApp1 (id, reduce0 v env)
  | FunApp (f, v) -> FunApp (reduce0 f env, reduce0 v env)
  | Lambda (ids, exp) -> 
    let e' = List.fold_left (fun e i -> IntSet.remove i e) env ids in
    Lambda (ids, reduce0 exp e')
  | Cond lst -> Cond (List.map (fun (x, y) -> (reduce0 x env, reduce0 y env)) lst)
  | Let (lst, exp) ->
    let env' = List.fold_left (fun ev par -> IntSet.remove (fst par) ev) env lst
    in Let 
      (List.map (function (id, va) -> (id, reduce0 va env)) lst, 
       reduce0 exp env')
  | LetRec (lst, exp) ->
    let env' = List.fold_left (fun ev par -> IntSet.remove (fst par) ev) env lst
    in LetRec
      (List.map (function (id, va) -> (id, reduce0 va env')) lst,
       reduce0 exp env')
  | Tuple lst -> Tuple (List.map (fun x -> reduce0 x env) lst)
  | ot -> ot
and reduceBinOp expr = reduce0 expr builtinFuncID
