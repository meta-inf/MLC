(* clousre.ml: build closures *)

open Cps
open Afx

module M =
struct
  type t = IntSet.t * (int IntMap.t)
  let add i (s, m) = 
    (assert(not (IntSet.mem i s));
     (IntSet.add i s,
      IntMap.add i (IntMap.cardinal m) m))
  let mem i (s, m) = IntSet.mem i s
  let find i (s, m) = IntMap.find i m
  let empty = (IntSet.empty, IntMap.empty)
  let cardinal (s, m) = IntSet.cardinal s
  let union (s0, m0) (s1, m1) = 
    let f _ y z = match y with Some _ -> y | None -> z in
    (IntSet.union s0 s1, IntMap.merge f m0 m1)
  let to_arr (s, m) = 
    let v = Array.of_list (IntMap.bindings m)
    in (Array.sort (fun (_, x) (_, y) -> compare x y) v;
        Array.map (fun (x, _) -> ID x) v)
end

let buildClosure =
  let rec trav cexp bound free = 
    match cexp with
    | CExp (ID i, ci) -> 
      let (free', ci') = trav0 ci (M.add i bound) free in
      (free', CExp (ID i, ci'))
  and trav0 cexp bound free =
    let free_ref = ref free in
    let updval = function
      | Var (ID i) -> 
        if not (M.mem i bound) then
          (
            if not (M.mem i (!free_ref)) then
              free_ref := M.add i (!free_ref);
            ClosureVal (ID (M.find i (!free_ref)))
          )
        else Var (ID i)
      | ClosureVal _ -> failwith "?.."
      | ret -> ret
    in
    match cexp with
    | CValue (ClosureVal _, _) -> failwith "..."
    | CValue (v, k) -> 
      let v' = updval v in
      let (free', k') = trav k bound (!free_ref) in
      (free', CValue (v', k'))
    | CTerm -> (free, CTerm)
    | CPrimOp1 (op, v, k) ->
      let v' = updval v in
      let (free', k') = trav k bound (!free_ref) in
      (free', CPrimOp1 (op, v', k'))
    | CPrimOp2 (op, v1, v2, k) ->
      let (v1', v2') = (updval v1, updval v2) in
      let (free', k') = trav k bound (!free_ref) in
      (free', CPrimOp2 (op, v1', v2', k'))
    | CCond (v, k1, k2) ->
      let v' = updval v in
      let (free', k1') = trav0 k1 bound (!free_ref) in
      let (free', k2') = trav0 k2 bound free' in
      (free', CCond (v', k1', k2'))
    | CFunApp (f, v) ->
      let (f', v') = (updval f, List.map updval v) in
      (!free_ref, CFunApp (f', v'))
    | CTuple (lst, k) ->
      let lst' = List.map updval lst in
      let (free', k') = trav k bound (!free_ref) in
      (free', CTuple (lst', k'))
    | CFix (lst, _, k) ->
      let bound' = 
        List.fold_left (fun s (ID i, _, _) -> M.add i s) M.empty lst
      in
      let tmp =
        List.fold_left
          (fun (lst0, f0) (ID i, lst, b) ->
             let bound' = List.fold_left 
                 (fun s (ID x) -> M.add x s) bound' lst in
             let (f', b') = trav0 b bound' f0 in
             ((ID i, lst, b') :: lst0, f')
          ) ([], M.empty) lst in
      let (lst', freeSet) = (List.rev (fst tmp), M.to_arr (snd tmp)) in
      let closure = Array.map (fun x -> updval (Var x)) freeSet in
      let (free', k') = trav0 k (M.union bound bound') (!free_ref) in
      (free', CFix (lst', closure, k'))
  in fun exp -> trav0 exp M.empty M.empty
