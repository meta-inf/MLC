open Afx
open Ast
open Types
open Env
open Pretype


module V = VEnv
module T = TEnv

type type_error_enum = Unification_error
                     | Invalid_POR
                     | TE_WTF

exception Type_error of type_error_enum


let rec unify: 'a type_tag -> 'a type_tag -> V.t -> unit
  = fun a b env -> 
    let a = V.expandVar a env in
    let b = V.expandVar b env in
    let (a, b) = if isVar b then (b, a) else (a, b) in
    if isVar b then (* isVar a *)
      begin
        V.mergeScp a b env;
        V.linkEqv a b env
      end
    else if isVar a then
      begin
        V.linkEqv a b env;
        iter_v 
          (fun v -> 
             if (V.getDep (TVar v) env) > (V.getDep a env) then 
               V.mergeScp a (TVar v) env)
          b;
      end
    else match (a, b) with
      | (TInt, TInt) | (TFloat, TFloat) | (TStr, TStr) | (TBool, TBool) | (TUnit, TUnit)
        -> ()
      | (TFunc (f1, v1), TFunc (f2, v2)) -> 
        (unify f1 f2 env; unify v1 v2 env)
      | (TTuple l1, TTuple l2) 
        when (List.length l1) = (List.length l2) -> 
        List.iter (fun (a, b) -> unify a b env) (List.combine l1 l2)
      | (TAlType (a1, l1), TAlType (a2, l2))
        when (a1 = a2) && ((List.length l1) = (List.length l2)) ->
        List.iter (fun (a, b) -> unify a b env) (List.combine l1 l2)
      | _ -> raise @@ Type_error Unification_error


let isRValue = function
  | Let _ | FunApp _ -> false
  | _ -> true

let rec typeOf e tenv venv = 
  match e with
  | IntConst _ -> TInt
  | FltConst _ -> TFloat
  | StrConst _ -> TStr
  | Var v -> V.instantiate (T.find v tenv) venv

  | FunApp (f, v) ->
    let u = V.newVar venv in
    let tf = typeOf f tenv venv in
    let tv = typeOf v tenv venv in
    (unify tf (TFunc (tv, u)) venv;
     u)

  | Func ([PVar v], e) -> (* v is of Var *)
    let tv = V.newVar venv in
    let tenv' = T.extend v (V.makeSig tv) tenv in
    let te = typeOf e tenv' venv in
    TFunc (tv, te)

  | Let (lbl, [(x, [], v)], e) ->
    begin
      V.createFrame venv;
      let tx = match lbl with
        | NonRec -> typeOf v tenv venv 
        | Rec -> 
          let tx0 = V.newVar venv in
          let te' = T.extend x (V.makeSig tx0) tenv in
          let tx1 = typeOf v te' venv in
          begin
            unify tx1 tx0 venv;
            tx0
          end
      in
      let tx = 
        if isRValue v then (V.finalize tx venv) else V.makeSig tx in
      begin
        V.exitFrame venv;
        let tenv' = T.extend x tx tenv in
        typeOf e tenv' venv 
      end
    end

  | Tuple lst ->
    TTuple (List.map (fun e -> typeOf e tenv venv) lst)

  | AlType (id, lst) ->
    let id = T.variantIDOf id in (
    match T.find id tenv with
    | (_, TFunc _) -> typeOf (FunApp (Var id, Tuple lst)) tenv venv
    | (_, TAlType _) -> typeOf (Var id) tenv venv
    | _ -> raise @@ Type_error Unification_error)

  | MatchExp (e, plist) ->
    let e_t = typeOf e tenv venv in
    let r_t = V.newVar venv in
    begin
      List.iter 
        (fun (pat, exp) ->
           let (tenv', pat_t) = loadPattern pat tenv venv in
           let exp_t = typeOf exp tenv' venv in
           begin
             unify pat_t e_t venv;
             unify exp_t r_t venv
           end)
        plist;
      r_t
    end

  | IfExp (cond, brTrue, optBrFalse) ->
    let condtype = typeOf cond tenv venv in
    let b1type = typeOf brTrue tenv venv in
    let b0type = match optBrFalse with
      | Some brFalse -> typeOf brFalse tenv venv 
      | None -> TUnit
    in
    begin
      unify condtype TBool venv;
      unify b1type b0type venv;
      b1type
    end

  | Grouped lst ->
    let types = List.map (fun x -> typeOf x tenv venv) lst in
    List.hd (List.rev types)

  | _ -> raise Not_implemented

and loadPattern pat tenv venv =
  let varlist = varListOfPat pat in (* check variable validity *)
  let tref = ref tenv in
  begin
    List.iter (* allocate type variables *)
      (fun s -> 
         let si = V.makeSig @@ V.newVar venv in
         tref := T.extend s si !tref)
      varlist;
    (!tref, typeOfPat pat !tref venv) (* typing *)
  end

and varListOfPat = 
  let rec trav = function
    | PInt _ | PFloat _ -> []
    | PVar s -> [s]
    | PTuple lst | PAlType (_, lst) -> List.concat (List.map trav lst)
    | PAlias (s, p) -> s :: (trav p)
    | POr (x, y) ->
      let vx = varListOfPat x in
      let vy = varListOfPat y in
      if not (vx = vy) then raise @@ Type_error Invalid_POR
      else vx
  in fun p -> let ret = trav p in List.sort_uniq compare ret

and typeOfPat pat tenv venv =
  match pat with
  | PInt _ -> TInt
  | PFloat _ -> TFloat
  | PVar s -> V.instantiate (T.find s tenv) venv
  | PTuple lst -> TTuple (List.map (fun p -> typeOfPat p tenv venv) lst)
  | PAlias (s, p) -> 
    let ptype = typeOfPat p tenv venv in
    let stype = V.instantiate (T.find s tenv) venv in
    begin
      unify ptype stype venv;
      ptype
    end
  | POr (x, y) ->
    let xt = typeOfPat x tenv venv in
    let yt = typeOfPat y tenv venv in
    begin
      unify xt yt venv;
      xt
    end
  | PAlType (id, lst) ->
    let id = T.variantIDOf id in (
    match T.find id tenv with
    | (_, TFunc _) -> 
      let arg_t = typeOfPat (PTuple lst) tenv venv in
      let arg_i = gensym () in
      let tenv' = T.extend arg_i (V.makeSig arg_t) tenv in
      typeOf (FunApp (Var id, Var arg_i)) tenv' venv 
    | (_, TAlType _) -> typeOf (Var id) tenv venv
    | _ -> raise @@ Type_error Unification_error)


let loadTypeDecl stmt tref =
  match stmt with
  | AlTypeDecl (id, param, conses) -> 
    (* TODO: check if id has been bound already *)
    let ret = TAlType (id, List.map (fun i -> TVar i) param) in
    List.iter (
      function
      | (cons, None) -> 
        tref := T.extend (T.variantIDOf cons) (param, ret) !tref
      | (cons, Some arg) -> 
        tref := T.extend (T.variantIDOf cons) (param, TFunc (arg, ret)) !tref)
      conses
  | _ -> raise @@ Type_error TE_WTF


type typing_result =
    TRExp of finalized type_sig
  | TRLet of (string * (finalized type_sig)) list
  | TRNone

let infer stmt tenv venv =
  match convertF stmt with
  | Expr e ->
    TRExp (V.finalize (typeOf e !tenv venv) venv)
  | AlTypeDecl _ -> 
    begin
      loadTypeDecl stmt tenv;
      TRNone
    end
  | GLetExp (Let (lbl, bnds, _)) ->
    let k' = List.map (fun (x, _, _) -> Var x) bnds in
    let nexp = Let (lbl, bnds, Tuple k') in
    let TTuple tags = typeOf nexp !tenv venv in
    let res = 
      List.map 
        (fun (Var id, tag) -> 
           let u = V.finalize tag venv in
           (tenv := T.extend id u !tenv;
            (id, u)))
        (List.combine k' tags)
    in TRLet res
  | _ -> failwith "infer"
