(* reduce0.ml: flatten calls to binary builtin functions 
 * *)

open Afx
open Ast
open Value

module IntSet = Set.Make(struct type t = int let compare = compare end)

let builtinFuncID = 
  let f = L.fold_left (fun tb id -> IntSet.add id tb) IntSet.empty 
  and g = L.map (function (x, y) -> Parse.IdTable.find x)
  in f (g builtinFuncList)

(* due to a bug in type inferer *)
let builtinFunc =
  let f = L.map (function (x, y) -> (Parse.IdTable.find x, y)) in
  let g lst = Array.init 
      ((L.fold_left max 0 (IntSet.elements builtinFuncID)) + 1)
      (fun id -> try L.assoc id (f lst) with Not_found -> Unit) in
  let arr1 = g builtinFuncList in fun id -> arr1.(id)

let binOpFunc = 
  let f = L.map (function (x, y) -> (Parse.IdTable.find x, y)) in
  let g lst = Array.init 
      ((L.fold_left max 0 (IntSet.elements builtinFuncID)) + 1)
      (fun id -> 
         try L.assoc id (f lst) 
         with Not_found -> snd @@ L.hd lst) in
  let arr0 = g builtinBinOps in fun id -> arr0.(id)


let rec reduce0 env expr =
  match expr with
  | FunApp (Identifier id, lst)
    when (Parse.IdTable.is_primitive id) && (not (IntSet.mem id env)) ->
      let ct = L.assoc id Parse.IdTable.paramcnt in
      let lst = L.map (reduce0 env) lst in
      begin
        assert(ct = (L.length lst));
        match lst with
        | [a] -> FunApp1 (id, a)
        | [a; b] when snd @@ Parse.IdTable.str_of_id id = "$" -> 
          reduce0 env @@ FunApp (a, [b]) (* (+) $ a $ b *)
        | [a; b] -> FunApp2 (id, a, b)
        | _ -> failwith "reduce0"
      end
  | FunApp (f, v) -> FunApp (reduce0 env f, L.map (reduce0 env) v)
  | Lambda (ids, exp) -> 
    let e' = L.fold_left (fun e i -> IntSet.add i e) env ids in
    Lambda (ids, reduce0 e' exp)
  | Cond lst -> 
    Cond (L.map (fun (x, y) -> (reduce0 env x, reduce0 env y)) lst)
  | Let (lst, exp) -> (* FIXME *)
    let env' = L.fold_left (fun ev par -> IntSet.add (fst par) ev) env lst
    in Let 
      (L.map (function (id, va) -> (id, reduce0 env va)) lst, 
       reduce0 env' exp)
  | LetRec (lst, exp) ->
    let env' = L.fold_left (fun ev par -> IntSet.add (fst par) ev) env lst
    in LetRec
      (L.map (function (id, va) -> (id, reduce0 env' va)) lst,
       reduce0 env' exp)
  | Tuple lst -> Tuple (L.map (reduce0 env) lst)
  | ot -> ot

let reduceBinOp e = reduce0 IntSet.empty e
