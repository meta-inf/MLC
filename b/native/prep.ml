(* prep.ml: prepare for cps convertion *)

open Afx
open Ast

let builtinNames = List.sort compare @@
  List.map (fun (s, _) -> Parse.IdTable.find s) Interp.Value.builtinFuncList

module IDSet = 
struct
  let queryID v (map, _) = IntMap.find v map
  let getNewID v (map, counter) = (* return (id, map') *)
    let c' = !counter
    in (counter := c' + 1;
        (c', (IntMap.add v c' map, counter)))
  let empty () = (* ensure a new set is constructed *)
    List.fold_left (fun t i -> snd @@ getNewID i t) (IntMap.empty, ref 0)
      builtinNames
end

let v = (IDSet.queryID 1 (IDSet.empty ())) == 0;;

let rec rename0 expr env = 
  match expr with
  | (IntConst _ | FltConst _ | BoolConst _ | StrConst _) -> expr
  | Identifier id -> Identifier (IDSet.queryID id env)
  | Cond lst -> 
    Cond (List.map (fun (cd, vl) -> (rename0 cd env, rename0 vl env)) lst)
  | FunApp (f, v) -> FunApp (rename0 f env, rename0 v env)
  | FunApp1 (i, v) -> (assert((IDSet.queryID i env) == i);
                       FunApp1 (i, rename0 v env))
  | FunApp2 (i, f, v) -> (assert((IDSet.queryID i env) == i);
                          FunApp2 (i, rename0 f env, rename0 v env))
  | Tuple lst -> Tuple (List.map (fun v -> rename0 v env) lst)
  (* cases that require renaming *)
  | Lambda (ids, vl) -> 
    let (ids', e') = 
      List.fold_left 
        (fun (l, e) i -> 
          let (i', e') = IDSet.getNewID i e in (i' :: l, e')) 
        ([], env) ids 
    in Lambda (List.rev ids', rename0 vl e')
  | Let (bnd, v) -> 
    let rec bnd0 = List.map (fun (id, vl) -> (id, rename0 vl env)) bnd
    and getbnd1 bnd env res = 
      match bnd with
      | [] -> (env, List.rev res)
      | (id, vl) :: ret ->
        let (id', e') = IDSet.getNewID id env
        in getbnd1 ret e' ((id', vl) :: res)
    in let (e', bnd') = getbnd1 bnd0 env []
    in Let (bnd', rename0 v e')
  | LetRec (bnd, v) -> 
    let e' = 
      List.fold_left (fun mp (id, _) -> snd @@ IDSet.getNewID id mp) env bnd
    in 
    LetRec (List.map (fun (id, vl) -> (IDSet.queryID id e', rename0 vl e')) bnd,
            rename0 v e')

let rename (expr: expr) = (* (expr, ref int (next usable id)) *) 
  let is = IDSet.empty () in 
  let e' = rename0 expr is in
  (e', snd is)
