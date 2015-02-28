open Ast
open IAst
open Afx
open PatAux

let collectTrait: Ast.pattern -> trait list =
  let rec trav pat ret pre loc =
    match pat with
    | PVar _ -> ()
    | PInt i -> ret := TNode (pre, loc, AInt i) :: !ret
    | PFloat f -> ret := TNode (pre, loc, AFloat f) :: !ret
    | PTuple lst -> 
      L.iteri (fun i sub -> trav sub ret pre ((i + 1) :: loc)) lst
    | PAlType (s, lst) ->
      let pre' = TNode (pre, 0 :: loc, ALabel s) in
      begin
        ret := pre' :: !ret;
        L.iteri (fun i sub -> trav sub ret pre' ((i + 1) :: loc)) lst
      end
    | PAlias (s, p) -> trav p ret pre loc
    | POr _ -> failwith ""
  in
  fun p ->  
    let res = ref [] in (trav p res TNil []; !res)


let poolOfTraits: ptrait list -> traitPool =
  fun lst -> 
    let f = ref IntMap.empty in
    begin
      L.iter 
        (fun x -> 
           let hpre = hash @@ preOfPack x in
           try
             let p = IntMap.find hpre !f in
             p := x :: !p
           with Not_found -> 
             f := IntMap.add hpre (ref [x]) !f)
        lst;
      let rec trav pre =
        let lst = 
          try !(IntMap.find (hash pre) !f) with Not_found -> [] 
        in
        let lst = L.map (fun x -> (x, trav @@ nodeOfPack x)) lst in
        Pool lst
      in trav TNil
    end

let markConflict: (trait list * int) list -> ptrait list =
  let conflict a b = match a, b with
    | TNode (_, p1, a1), TNode (_, p2, a2) when p1 = p2 -> not (a1 = a2)
    | _ -> false
  in
  fun inp -> 
    let traits = 
      L.concat (L.map fst inp) 
      |> L.sort_uniq (fun a b -> compare (hash a) (hash b))
    in
    let mark t =
      let tmp = 
        L.map 
          (fun (l, i) -> 
             if L.mem t l then (i, 0)
             else if L.exists (conflict t) l then (i, 1)
             else (i, 2))
          inp
      in
      (tmp |> L.filter (fun x -> (snd x) = 0) |> L.map fst,
       tmp |> L.filter (fun x -> (snd x) = 1) |> L.map fst,
       tmp |> L.filter (fun x -> (snd x) = 2) |> L.map fst)
    in
    L.map (
      fun t ->
        let has, con, rst = mark t in
        Pack (t, has @ rst, con @ rst))
      traits

(* Find the next trait to test in (Pool lst), on condition that
 * only patterns in curpats are left to check. *)
let getTrait lst curpats =
  let lst' = 
    L.map (
      fun (Pack (p, a, b), s) -> 
        (Pack (p, set_union curpats a, set_union curpats b), s))
      lst
  in
  let eval a b =
    (L.length (set_union a b), max (L.length a) (L.length b))
  in
  let ret = 
    L.fold_left 
      (fun ((Pack (p, a, b), _) as ans) ((Pack (p', a', b'), _) as cur) ->
         let vAns, vCur = (eval a b), (eval a' b') in
         if vCur < vAns then cur else ans)
      (Pack (TNil, curpats @ [-1], curpats @ [-1]), Pool [])
      lst'
  in (ret, lst')

(* Build the decision tree; id is matchee's name *)
let translate: string -> (Ast.pattern * IAst.iexpr) list -> IAst.iexpr =
  fun id inp ->
    let pat = L.mapi (fun i (a, _) -> (collectTrait a, i)) inp in
    let pool = poolOfTraits @@ markConflict pat in

    let rec trav (Pool lst) cpat checked penv ienv =
    (* `Pool lst`: pool of available traits, roots of which is safe to test,
     * `cpat`    : candidate pattern set, 
     * `checked` : traits that have been checked,
     * `penv`    : generate variable for access path,
     * `ienv`    : generate il expression *)
      if L.length cpat = 0 then IE.match_failure ienv
      
      else if L.length lst = 0 then 
        (* everything is checked. *)
        let pid = L.fold_left min max_int cpat in
        IE.match_success pid penv ienv

      else if L.length cpat = 1 then
        (* Finish unchecked traits. *)
        let pid = L.hd cpat in
        let (traits, _) = L.nth pat pid in
        let unchecked = L.filter (fun x -> not (L.mem x checked)) traits in
        let penv', tf = 
          L.fold_right 
            (fun cur (penv, tf) ->
               let TNode (_, path, aval) = cur in
               let penv', tf0, pathS = PE.get_path path penv in
               (penv', 
                fun e -> tf @@ tf0 @@ IAst.(makeIf (eqvAValue aval pathS, 
                                                    e, 
                                                    IE.match_failure ienv))))
            unchecked
            (penv, Afx.id)
        in tf (IE.match_success pid penv' ienv)

      else
        let (pack, Pool sons), pool' = getTrait lst cpat in
        let TNode (_, path, aval) = nodeOfPack pack in
        let ptraits = match aval with (* trait to be checked *)
          | ALabel _ -> 
            L.filter (fun (p, _) -> pathOfPack p = path) pool'
          | _ -> [(pack, Pool sons)]
        in
        let traits = ptraits |> L.map (fun (x, _) -> nodeOfPack x) in
        let pool' = 
          L.filter (fun (x, _) -> not (L.mem (nodeOfPack x) traits)) pool' in
        let penv', tf, pathS = PE.get_path path penv in
        let condparam =
          L.map 
            (function (Pack (TNode (_, _, aval), strue, _), Pool sons) -> 
              (eqvAValue aval pathS, 
               trav (Pool (pool' @ sons)) strue (traits @ checked) penv' ienv))
            ptraits
          @
          [IAst.BoolConst true,
           let cpat' = 
             L.map (function (Pack (_, _, sf), _) -> sf) ptraits 
             |> L.fold_left set_union cpat
           in trav (Pool pool') cpat' (traits @ checked) penv' ienv]
        in tf (Cond condparam)

    in
    let pe, ie = PE.init id, IE.init id inp in
    let dec = trav pool (L.mapi (fun i _ -> i) inp) [] pe ie in
    IE.finalize ie dec 

(*
let expandPOr: ((Ast.pattern * Ast.expr) list) -> 
  ((Ast.pattern * Ast.expr) list * (Ast.expr -> Ast.expr)) =
  let rec expandPat = function
    | PInt _ | PFloat _ | PVar _ as v -> [v]
    | PTuple lst -> 
      expandPat lst 
      |> L.fold_left 
        (fun cur elm -> L.map (fun x -> x :: cur) elm)
        []
      |> L.rev_map (fun x -> PTuple x)
    | PAlType (v, lst) -> 
      expandPat lst
      |> L.fold_left
        (fun cur elm -> L.map (fun x -> x :: cur) elm)
        []
      |> L.rev_map (fun x -> PAlType (v, x))
    | POr (x, y) -> (expandPat x) @ (expandPat y)
    | PAlias (s, x) -> expandPat x |> L.map (fun x -> PAlias (s, x))
  in 
  let getBrPair =
    let c = ref 0 in
    fun () -> (c := !c + 1; 
               (Printf.sprintf "matchbr/%d" !c,
                Printf.sprintf "mbrnull/%d" !c))
  in fun lst -> 
    let bindings = ref [] in
    let lst0 = 
      L.map 
        (fun (p, v) -> 
           let p' = expandPOr p in
           if p' = [p] then [(p, v)]
           else 
             let brID, xID = getBrPair () in
             TODO: It's wrong. We need the closure info.
             (bindings := (brID, [], Func ([PVar xID], v)) :: !bindings;
              L.map (fun p -> (p, FunApp (Var brID, IntConst 0))) p'))
        lst
      |> L.concat
    in
    let tf = 
      if !bindings = [] then (fun x -> x)
      else (fun x -> Ast.Let (NonRec, !bindings, x))
    in (lst0, tf)
*)
