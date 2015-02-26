(* cps.ml: convert source into cps style *)

open Afx

type id = ID of int

type value = Var of id
           | ClosureVal of id
           | Int of int
           | Float of float
           | Bool of bool
           | Str of string
           | Unit


type cimpl = CValue of value * cexp
           | CPrimOp1 of int * value * cexp
           | CPrimOp2 of int * value * value * cexp
           | CCond of value * cimpl * cimpl
           | CFunApp of value * (value list)
           | CTuple of (value list) * cexp
           | CFix of (id * (id list) * cimpl) list * (value array) * cimpl
           | CTerm
and cexp = CExp of id * cimpl

type dimpl = DValue of value * dexp
           | DPrimOp1 of int * id * dexp
           | DPrimOp2 of int * id * id * dexp
           | DCond of id * dimpl * dimpl
           | DFunApp of id * (id list)
           | DTuple of (id list) * dexp
           (* Fix: ((bndID, args (including k), body) lst, cont) *)
           | DFix of (id * (id list) * dimpl) list * dimpl
           | DTerm
and dexp = DExp of id * dimpl

module C =
struct
  let make init = ref init
  let get v = (v := (!v) + 1; ID !v)
  let get2 v = (get v, get v)
  let get3 v = (get v, get v, get v)
end


let rec trans0 (exp: Ast.expr) (ct: int ref) (k: dexp) = 
  let trans exp k = trans0 exp ct k in
  match exp with
  | Ast.IntConst v -> DValue (Int v, k)
  | Ast.FltConst v -> DValue (Float v, k)
  | Ast.BoolConst v -> DValue (Bool v, k)
  | Ast.StrConst v -> DValue (Str v, k)
  | Ast.Identifier i -> DValue (Var (ID i), k)

  | Ast.Cond lst -> (
    match k with
    | DExp (_, DFunApp (_, _)) ->
      List.fold_right
        (fun (cond, res) cimpl ->
           let ci = C.get ct in
           trans cond @@ DExp (ci, DCond (ci, trans res k, cimpl)))
        lst
        (DValue (Unit, k))
    | _ ->
      let DExp (kvi, kimpl) = k in
      let ki, ri = C.get2 ct in
      DFix ([ki, [kvi], kimpl], trans exp (DExp (ri, DFunApp (ki, [ri])))))

  | Ast.FunApp1 (id, va) ->
    let vid = C.get ct
    in trans va @@ DExp(vid, DPrimOp1(id, vid, k))
  | Ast.FunApp2 (id, vl, vr) ->
    let (li, ri) = C.get2 ct
    in trans vl @@
    DExp(li, trans vr @@ DExp(ri, DPrimOp2(id, li, ri, k)))
  | Ast.Tuple lst ->
    let vlst = List.map (fun e -> (e, C.get ct)) lst in
    let imp0 = DTuple(List.map snd vlst, k) in
    List.fold_right (fun (e, i) impl -> trans e (DExp(i, impl))) vlst imp0

  | Ast.Let ((id, vl) :: rest, body) ->
    trans vl (DExp (ID id, trans (Ast.Let (rest, body)) k))
  | Ast.Let ([], body) -> trans body k

  | Ast.FunApp (f, v) -> 
    let DExp (kvi, kimpl) = k in
    let (ki, fi, vi) = C.get3 ct in
    DFix ([(ki, [kvi], kimpl)], 
          trans f @@ DExp (fi, trans v @@ DExp (vi, DFunApp (fi, [vi; ki]))))

  | Ast.Lambda (args, v) -> 
    (* li: the lambda expr; k0: its cont.; vi: the result of fun body *)
    let (li, k0, vi) = C.get3 ct in
    let argsI = List.map (fun i -> ID i) args in
    DFix ([(li, argsI @ [k0], trans v @@ DExp (vi, DFunApp (k0, [vi])))],
          DValue (Var li, k))

  | Ast.LetRec (bindings, body) ->
    DFix (List.map (
        fun (bndID, body) -> 
          match body with
          | Ast.Lambda (args, expi) ->
            let k0, vi = C.get2 ct in
            let argsI = List.map (fun i -> ID i) args in
            (ID bndID, argsI @ [k0], 
             trans expi @@ DExp (vi, DFunApp (k0, [vi])))
          | _ -> failwith "letrec - only explicit functions are allowed"
      ) bindings, trans0 body ct k)

let trans (exp, ct) = trans0 exp ct (DExp (C.get ct, DTerm))
