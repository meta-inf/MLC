open Afx
open Types

module IM = IntMap
module SM = StrMap

module TEnv = 
struct
  type t = finalized type_sig SM.t

  let empty = SM.empty
  let extend = SM.add
  let find = SM.find
               
  let _extends lst typ set = 
    List.fold_left (fun s a -> extend a typ s) set lst

  let variantIDOf str = 
    Printf.sprintf "variant/%s" str

  let init = 
    empty 
    |> _extends ["+"; "-"; "*"; "/"] ([], TFunc (TInt, TFunc (TInt, TInt)))
    |> _extends ["="; "=="; ">"; "<"] ([1], TFunc (TVar 1, TFunc (TVar 1, TBool)))
    |> _extends [variantIDOf "list_empty"] ([1], TAlType ("list", [TVar 1]))
    |> _extends [variantIDOf "list"] 
      ([1], TFunc (TTuple [TVar 1; TAlType ("list", [TVar 1])], 
                   TAlType ("list", [TVar 1])))
    |> _extends ["::"] 
      ([1], TFunc (TVar 1, TFunc (TAlType ("list", [TVar 1]),
                                  TAlType ("list", [TVar 1]))))
    |> _extends ["dispi"] ([], TFunc (TInt, TUnit))
end


module VEnv =
struct
  type t =
    {
      mutable h2t : instantiated type_tag IM.t;
      mutable scpDep : int IM.t;
      mutable scpDsu : int IM.t;
      mutable eqvDsu : int IM.t;
      mutable curDep : int;
      mutable varCnt : int;
    }
  
  let empty () = { h2t = IM.empty; 
                   scpDep = IM.empty; 
                   scpDsu = IM.empty; 
                   eqvDsu = IM.empty;  
                   curDep = 0;
                   varCnt = 0; }

  let hash = Hashtbl.hash
  
  let newVar e =
    let id = e.varCnt in
    let res = TVar id in
    let resHash = hash res in
    begin
      e.varCnt <- id + 1;
      e.scpDsu <- IM.add resHash resHash e.scpDsu;
      e.eqvDsu <- IM.add resHash resHash e.eqvDsu;
      e.scpDep <- IM.add resHash e.curDep e.scpDep;
      e.h2t <- IM.add resHash res e.h2t;
      res
    end

  let rec _find s a =
    let b = IM.find a !s in
    if a = b then a
    else
      let f = _find s b in
      (s := IM.add a f !s; f)

  let expandVar t e =
    if isVar t then
      let rhash = _find (ref e.eqvDsu) (hash t) in
      IM.find rhash e.h2t
    else t

  let mergeScp a b e =
    let st = ref e.scpDsu in
    let fa = _find st (hash a) in
    let fb = _find st (hash b) in
    let (x, y) = 
      if IM.find fa e.scpDep > IM.find fb e.scpDep then (fa, fb)
      else (fb, fa)
    in
    e.scpDsu <- IM.add x y !st

  let linkEqv a b e = 
    let st = ref e.eqvDsu in
    let fa = _find st (hash a) in
    begin
      assert(isVar a);
      if not (isVar b) then
        let hash_b = hash b in
        begin
          e.h2t <- IM.add hash_b b e.h2t;
          e.eqvDsu <- !st |> IM.add hash_b hash_b |> IM.add fa hash_b
        end
      else
        let fb = _find st (hash b) in
        e.eqvDsu <- !st |> IM.add fa fb
    end
    
  let getDep : 'a type_tag -> t -> int
    = fun a e ->
      let st = ref e.scpDsu in
      let fa = _find st (hash a) in
      begin
        e.scpDsu <- !st;
        IM.find fa e.scpDep
      end

  let createFrame e = e.curDep <- e.curDep + 1
  let exitFrame e = e.curDep <- e.curDep - 1

  let makeSig tag =
    ([], tag_finalize tag)

  let rec finalize : instantiated type_tag -> t -> finalized type_sig =
    fun tag venv ->
      let free = ref IntSet.empty in
      let rec trav t =
        let t = expandVar t venv in
        match t with
        | TInt | TFloat | TBool | TUnit -> t
        | TVar v -> 
          begin
            if getDep (TVar v) venv == venv.curDep then
              free := IntSet.add v !free
            else ();
            t
          end
        | TFunc (f, v) -> TFunc (trav f, trav v)
        | TTuple lst -> TTuple (List.map trav lst)
        | TAlType (a, lst) -> TAlType (a, List.map trav lst)
      in
      let tag' = tag_finalize @@ trav tag in
      (IntSet.elements !free, tag')

  let rec instantiate : finalized type_sig -> t -> instantiated type_tag =
    fun (free, tag) venv ->
      let subst = List.map (fun v -> (v, newVar venv)) free in
      map_v 
        (fun v ->
           try 
             let TVar id = List.assoc v subst in id
           with Not_found -> v)
        (tag_instantiate tag)

end
