(* Prereg.ml: transform cexp to mexp. translate fix, rename temp var, etc
 * Optimizations should be finished before this pass. 
 * *)

open Afx
open Cps
open Mvalue


module VarMap = (* ID (in Ast) -> mvalue *)
struct
  type t = (mvalue IntMap.t * int) ref

  let empty () = ref (IntMap.empty, 0)

  let translateID (ID v) x = 
    let (tbl, cnt) = !x in
    try IntMap.find v tbl with Not_found -> 
      let ret = StkVar cnt in 
      (x := (IntMap.add v ret tbl, cnt + 1);
       ret)

  let newStkID x =
    let (tbl, cnt) = !x in
    let ret = cnt in (x := (tbl, cnt + 1); ret)

  let findID (ID x) m = IntMap.find x (fst !m)

  let assign x v m = 
    let (tbl, cnt) = !m in
    (assert (try (IntMap.find x tbl; false) with Not_found -> true);
     m := (IntMap.add x v tbl, cnt))

  let replace x v m = 
    let (tbl, cnt) = !m in
    m := (IntMap.add x v tbl, cnt)
end

(* mark the maximum heap memory used by a procedure, for the garbage collector *)
let rec markMem (e: 'a mexp) = 
  match e with
  | MPrimOp1 (_, _, _, _, k) -> markMem k
  | MPrimOp2 (_, _, _, _, _, k) -> markMem k
  | MCond (_, _, k1, k2) -> max (markMem k1) (markMem k2)
  | MFunApp _ -> 0
  | MTuple (_, _, lst, k) -> (List.length lst) + (markMem k)
  | MCopy (_, _, _, k) -> markMem k
  | MTerm -> 0

(* flatten: cexp -> int Prog.t *)
let flatten = 
  let rec flatten cexp progref vmap = 
    let getStkVar = VarMap.translateID
    in 
    let translate = function (* cvalue -> mvalue; possibly register strings *)
      | Var i -> VarMap.findID i vmap
      | ClosureVal (ID i) -> CloVar i
      | Int i -> Imm i
      | Float i -> ImmF i
      | Bool i -> Imm (if i then 1 else 0)
      | Str s -> ImmS (Prog.addStr s progref)
      | Unit -> Imm 0 (* TODO: boxed *)
    and flatten0 x = flatten x progref vmap in
    let updateArgs args =
      let (f, args1r) = 
        List.fold_left 
          (fun (cur, lst) mv ->
             match mv with
             | CloRef (pos, None, fID) -> 
               let nID = VarMap.newStkID vmap in
               begin
                 VarMap.replace fID (CloRef (pos, Some (StkVar nID), fID)) vmap;
                 ((fun e -> MTuple (0, StkVar nID, [Imm 0; pos; Label fID], cur e)),
                  (StkVar nID) :: lst)
               end
             | CloRef (_, Some v, _) -> (cur, v :: lst)
             | _ -> (cur, mv :: lst))
          ((fun x -> x), [])
          (List.map translate args)
      in (List.rev args1r, f)
    in

    match cexp with
    | CValue _ -> failwith "..." (* Cps.Reduce *)
    | CTerm -> MTerm

    | CPrimOp1 (id, v1, CExp (id2, k)) -> 
      let ([v1'], con) = updateArgs [v1] in
      let dst = getStkVar id2 vmap in
      con @@ MPrimOp1 (0, id, dst, v1', flatten0 k)

    | CPrimOp2 (id, v1, v2, CExp (id2, k)) ->
      let ([v1'; v2'], con) = updateArgs [v1; v2] in
      let dst = getStkVar id2 vmap in
      con @@ MPrimOp2 (0, id, dst, v1', v2', flatten0 k)

    | CCond (v, k1, k2) ->
      MCond (0, translate v, flatten0 k1, flatten0 k2)

    | CFunApp (v, lst) ->
      let (lst', con) = updateArgs lst in
      con @@ MFunApp (0, translate v, lst')

    | CTuple (lst, CExp (id, k)) ->
      let (lst', con) = updateArgs lst in
      let dst = getStkVar id vmap in
      con @@ MTuple (0, dst, lst', flatten0 k)

    | CFix (lst, clo, k) ->
      let mapK = VarMap.empty () in (* init vmap for letrec binding *)
      let () = List.iter 
          (fun (ID i, _, _) -> VarMap.assign i (CloRef (Reg 15, None, i)) mapK)
          lst
      in
      let cloID = VarMap.newStkID vmap in
      begin
        List.iter (* push function impl to progref *)
          (fun (ID id, args, k) -> 
             begin
               let nvmap = ref !mapK in (* load args to stack for func ID *)
               let () = List.iter (fun id -> ignore @@ getStkVar id nvmap) args
               in
               let impl = flatten k progref nvmap in
               Prog.addProc (Proc (id, snd !nvmap, markMem impl, List.length args, impl)) progref;
             end)
          lst;
        List.iter (* register functions for letrec body *)
          (fun (ID fid, _, _) ->
             VarMap.assign fid (CloRef (StkVar cloID, None, fid)) vmap)
          lst;
        let (clo', con) = updateArgs @@ Array.to_list clo in
        let f k = (* evaluation order *)
          con @@ MTuple (0, StkVar cloID, Imm 0 :: clo', k)
        in f (flatten0 k)
      end

  in fun cexp ->
    let progref, vmap = ref @@ Prog.empty (), VarMap.empty () in
    let main = flatten cexp progref vmap in
    (Prog.addProc (Main (snd !vmap, markMem main, main)) progref;
     !progref)
