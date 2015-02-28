open Ast
open Afx
open IAst
open Patcomp
    
let newID =
  let ct = ref 0 in
  fun () -> begin ct := !ct + 1; sprintf "mat/%d" !ct end

let rec transBnds x =
  L.map 
    (fun (s, b, e) -> 
       match b with
       | [] -> (s, trans0 e)
       | _ -> raise Not_implemented)
    x
           
and trans0: expr -> iexpr =
  function
    | Ast.IntConst i -> IAst.IntConst i
    | Ast.FltConst i -> IAst.FltConst i
    | Ast.StrConst i -> IAst.StrConst i
    | Ast.Var i -> IAst.Identifier i
    | Ast.Tuple ls -> 
      IAst.Tuple ((IAst.IntConst 0) :: (L.map (trans0) ls))
    | Ast.AlType (s, ls) ->
      IAst.Tuple ((IAst.IntConst (Env.findOrAdd s)) :: (L.map trans0 ls))
    | Ast.FunApp (f, v) ->
      IAst.FunApp (trans0 f, [trans0 v])
    | Ast.Func (lst, body) ->
      IAst.Lambda (L.map (fun (PVar s) -> s) lst, trans0 body)
    | Ast.IfExp (cond, br0, Some br1) ->
      IAst.makeIf (trans0 cond, trans0 br0, trans0 br1)
    | Ast.IfExp (cond, br0, None) ->
      IAst.Cond [trans0 cond, trans0 br0]

    | Ast.Let ((pat, v), k) ->
      trans0 (Ast.MatchExp (v, [pat, k]))

    | Ast.LetRec (bindings, value) ->
      IAst.LetRec (transBnds bindings, trans0 value)

    | Ast.MatchExp (exp, patterns) -> (
      let pat = L.map (fun (x, y) -> (x, trans0 y)) patterns in
      match exp with
      | Var s -> Patcomp.translate s pat
      | _ -> 
        let nid = newID () in
        IAst.Let ([nid, trans0 exp], Patcomp.translate nid pat))

    | Ast.Grouped lst -> IAst.Seq (L.map trans0 lst)


let translate stmts =
  let aux a b =
    match a with
    | AlTypeDecl _ -> b
    | Expr e -> IAst.Seq [trans0 e; b]
    | GLetExp (Let ((p, v), _)) -> 
      let nid = newID () in
      IAst.Let ([nid, trans0 v], Patcomp.translate nid [p, b])
    | GLetExp (LetRec (bnd, _)) -> 
      IAst.LetRec (transBnds bnd, b)
    | _ -> failwith ""
  in Uncurry.uncurry @@ L.fold_right aux stmts (IAst.IntConst 0)

