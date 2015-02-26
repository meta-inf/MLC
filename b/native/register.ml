(* Register.ml: arrange registers *)

open Afx
open Mvalue


module FM = (* FreqMap *)
struct
  type t = mvalue IntMap.t * int MVMap.t * int
  (* (pque, assoc, timestamp) *)

  let empty = (IntMap.empty, MVMap.empty, 0)

  let _inc (v: mvalue) ((pque, assoc, ctime): t) =
    let q0 = 
      try 
        let t = MVMap.find v assoc in 
        IntMap.remove t pque
      with Not_found -> pque
    in 
    let pque' = IntMap.add (-ctime) v q0 in 
    let assoc' = MVMap.add v (-ctime) assoc in
    (pque', assoc', ctime + 1)

  let inc v m = 
    varMatch (fun x -> _inc x m) (fun x -> m) v

  let _nextTime (i: mvalue) ((_, m, _): t) = 
    try MVMap.find i m with Not_found -> max_int

  let alive (v: mvalue) (m: t) =
    varMatch (fun x -> _nextTime x m < max_int) (fun _ -> false) v

  (* return the value whose first occurance is farthest from now.
   * `lst` should be the list of register content *)
  let findLast (lst: mvalue list) (m: t) = 
    let calc i = (_nextTime i m, i) in
    snd @@
    List.fold_left
      (fun x i -> max x (calc i)) (calc (List.hd lst))
      (List.tl lst)
end

let rec getFreq0 = function
    | MPrimOp1 (m, _, _, _, _) -> m
    | MPrimOp2 (m, _, _, _, _, _) -> m
    | MCond (m, _, _, _) -> m
    | MFunApp (m, _, _) -> m
    | MTuple (m, _, _, _) -> m
    | MTerm -> FM.empty
    | _ -> raise (Invalid_argument "getfreq")

let rec markFreq (mexp: int mexp) = 
  match mexp with
  | MPrimOp1 (_, id, dst, src, k) ->
    let k' = markFreq k in
    MPrimOp1 (FM.(getFreq0 k' |> inc dst |> inc src),
              id, dst, src, k')
  | MPrimOp2 (_, id, dst, s1, s2, k) ->
    let k' = markFreq k in
    MPrimOp2 (FM.(getFreq0 k' |> inc dst |> inc s1 |> inc s2),
              id, dst, s1, s2, k')
  | MCond (_, id, k1, k2) ->
    let (k1', k2') = (markFreq k1, markFreq k2) in
    MCond (FM.(getFreq0 k1' |> inc id), id, k1', k2')
  | MFunApp (_, id, lst) ->
    MFunApp (FM.empty, id, lst)
  | MTuple (_, dst, lst, k) ->
    let k' = markFreq k in
    MTuple (getFreq0 k', dst, lst, k')
  | MTerm -> MTerm
  | MCopy _ -> raise @@ Invalid_argument "markFreq"


module RM = (* register map *)
struct
  type t = int MVMap.t * IntSet.t (* (regmap, free-reg-set) *)

  let allowedRegList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]

  let empty = (MVMap.empty, IntSet.of_list allowedRegList)

  let _find mv (tbl, _) = MVMap.find mv tbl
  let _loaded mv (tbl, _) = MVMap.exists (fun a _ -> a = mv) tbl

  let _alloc mv (tblref: t ref) =
    begin
      assert(not (_loaded mv !tblref));
      let (tbl, set) = !tblref in
      let reg = IntSet.min_elt set in
      begin
        tblref := (MVMap.add mv reg tbl, IntSet.remove reg set);
        reg
      end
    end

  let _spill mv (tblref: t ref) =
    begin
      assert(_loaded mv !tblref);
      let (tbl, set) = !tblref in
      let reg = MVMap.find mv tbl in
      begin
        tblref := (MVMap.remove mv tbl, IntSet.add reg set);
        reg
      end
    end

  let find mv tbl = varMatchNC 
      (fun mv -> try Reg (_find mv tbl) with Not_found -> mv) 
      Afx.id
      mv

  let occupied reg tbl = not @@ IntSet.mem reg (snd tbl)
  let elem reg tbl = 
    fst @@ MVMap.choose @@ MVMap.filter (fun _ b -> b = reg) (fst tbl)

  let full tbl = 
    MVMap.cardinal (fst tbl) = (List.length allowedRegList)

  let loaded mv tbl = 
    varMatchNC (fun mv -> _loaded mv tbl) (fun _ -> true) mv

  let alloc mv tblref = 
    varMatchNC (fun mv -> Reg (_alloc mv tblref)) (fun _ -> failwith "") mv

  let spill mv tblref = 
    varMatchNC (fun mv -> Reg (_spill mv tblref)) (fun _ -> failwith "") mv

  let aliveList (tbl, _) = List.map fst @@ MVMap.bindings tbl
  let aliveRegList (tbl, _) = List.map snd @@ MVMap.bindings tbl
end


let rec registerize0 mexp rmap =
  let rmref = ref rmap in
  let find a = RM.find a !rmref in
  let load mvlist = (* to register *)
    List.fold_left 
      (fun ret (v, doCopy) ->
         if RM.loaded v !rmref then ret
         else if not (RM.full !rmref) then
           let r = RM.alloc v rmref in
           if doCopy then
             (fun x -> MCopy (!rmref, r, v, ret x)) 
           else ret
         else
           let p = FM.findLast (RM.aliveList !rmref) (getFreq0 mexp) in
           let r' = RM.spill p rmref in
           let r = RM.alloc v rmref in
           if doCopy then
             (fun x -> MCopy (!rmref, p, r', MCopy (!rmref, r, v, ret x)))
           else
             (fun x -> MCopy (!rmref, p, r', ret x))
      )
      (fun x -> x)
      mvlist
  in
  let reg mexp = registerize0 mexp !rmref
  in
  let buildIDIV fm id dst s0 s1 k isDiv =
    let ocp0, ocp3 = RM.occupied 0 rmap, RM.occupied 3 rmap in
    let find2 = (* perform find as if Reg0 & 3 were deallocated. *)
      varMatchNC
        (function
          | x when ocp0 && (x = RM.elem 0 rmap) -> x (* rmap is evaled here *)
          | x when ocp3 && (x = RM.elem 3 rmap) -> x
          | x -> RM.find x !rmref)
        Afx.id
    in
    let k' = reg k
    in let k' = (* 3. restore Reg0 & 3 *)
         if ocp3 then MCopy (!rmref, Reg 3, RM.elem 3 rmap, k') else k'
    in let k' = 
         if ocp0 then MCopy (!rmref, Reg 0, RM.elem 0 rmap, k') else k'
    in let k' = (* 2. perform idiv, save the result *)
         MCopy (!rmref, Reg 3, Imm 0,
         MCopy (!rmref, Reg 0, find2 s0,
         MPrimOp1 (!rmref, id, find2 dst, find2 s1, 
         MCopy (!rmref, find2 dst, (if isDiv then Reg 0 else Reg 3), k'))))
    in let k' = (* 1. backup Reg0 & 3 *)
         if ocp0 then MCopy (!rmref, RM.elem 0 rmap, Reg 0, k') else k'
    in let k' = 
         if ocp3 then MCopy (!rmref, RM.elem 3 rmap, Reg 3, k') else k'
    in k'
  in
  let spillAllRegs () =
    let regs = RM.aliveRegList rmap in
    let f = List.fold_left 
        (fun f r -> 
           let v = RM.elem r !rmref in
           begin
             ignore @@ RM.spill v rmref;
             (fun k -> MCopy (!rmref, v, Reg r, f k))
           end)
        (fun x -> x)
        regs
    in f
  in

  match mexp with
  | MPrimOp1 (fm, id, dst, src, k) ->
    let f = load [src, true; dst, false] in (* update rmref first *)
    f (MPrimOp1 (!rmref, id, find dst, find src, reg k))

  | MPrimOp2 (fm, id, dst, s0, s1, k) ->
    if (id = Parse.IdTable.find "/") then 
      buildIDIV fm id dst s0 s1 k true
    else 
      let f = load [dst, false; s0, true; s1, true] in
      f (MPrimOp2 (!rmref, id, find dst, find s0, find s1, reg k))

  | MCond (fm, id, k1, k2) ->
    let f = load [id, true] in
    f (MCond (!rmref, find id, reg k1, reg k2))

  | MFunApp (fm, id, args) -> (
    match id with
    | (StkVar _ | CloVar _) -> 
      let f = load [id, true] in
      f (MFunApp (!rmref, find id, List.map find args))
    | _ -> 
      MFunApp (!rmref, id, List.map find args))

  | MTuple (fm, id, lst, k) -> 
    let f = spillAllRegs () in
    let ret = MTuple (!rmref, id, List.map find lst, reg k) in
    f ret

  | MTerm -> MTerm
  | MCopy _ -> raise @@ Invalid_argument "registerize0"


let registerize ((smap, lst): int Prog.t) =
  (smap,
   List.map (
     function 
     | Proc (i, c, h, a, j) -> Proc (i, c, h, a, registerize0 (markFreq j) RM.empty)
     | Main (c, h, j) -> Main (c, h, registerize0 (markFreq j) RM.empty))
     lst)
