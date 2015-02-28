open Afx
open IAst

let rec uncurry0 env expr =
  let find id = try L.assoc id env with Not_found -> 1 in
  match expr with
  | IntConst _ | FltConst _ | BoolConst _ | StrConst _ | Identifier _ -> expr

  | FunApp (s, [e]) -> (
    match uncurry0 env s with 
    | FunApp (Identifier id, r)
      when find id > (L.length r) -> 
      FunApp (Identifier id, r @ [uncurry0 env e])
    | ot ->
      FunApp (ot, [uncurry0 env e]))
  | FunApp (_, _) -> failwith "uncurry0"

  | Lambda (ids, exp) -> 
    let env' = L.fold_left (fun a b -> (b, 1) :: a) env ids in
    Lambda (ids, uncurry0 env' exp)

  | Cond lst -> Cond (L.map (fun (x, y) -> (uncurry0 env x, uncurry0 env y)) lst)

  | LetRec (lst, body) ->
    let env' = 
      L.fold_left 
        (fun env (id, exp) ->
           match exp with
           | Lambda (lst, _) -> (id, L.length lst) :: env
           | _ -> env)
        env lst
    in
    LetRec (L.map (fun (i, e) -> (i, uncurry0 env' e)) lst, uncurry0 env' body)

  | Let (lst, body) -> (* treating as nested *)
    let lst', env' = 
      L.fold_left 
        (fun (bnds, env) (id, exp) ->
           let env' = 
             match exp with
             | Lambda (lst, _) -> (id, L.length lst) :: env
             | _ -> env
           in (bnds @ [id, uncurry0 env exp], env'))
        ([], env) lst
    in
    Let (lst', uncurry0 env' body)

  | Tuple lst -> Tuple (L.map (uncurry0 env) lst)
  | Seq lst -> Seq (L.map (uncurry0 env) lst)
                   

let rec expandBnd (id, e) = 
  let rec expandparam (lst, k) =
    match lst with
    | [] -> k
    | a :: rs -> Lambda ([a], expandparam (rs, k))
  in
  match e with
  | Lambda (lst, k) when L.length lst > 1 -> 
    [id ^ "$", Lambda (lst, k);
     id, expandparam (lst, 
                      IAst.(FunApp (Identifier (id ^ "$"), 
                                   L.map (fun s -> Identifier s) lst)))]
  | ot -> [id, ot]

and uncurry1 env expr =
  let find id = try L.assoc id env with Not_found -> 1 in
  let not_primitive id = 
    not (Env.is_primitive id) || (L.length (L.filter (fun (a, b) -> a = id) env) > 1)
  in
  let rec expandFunApp (f, a) =
    match a with
    | b :: [] -> FunApp (f, [b])
    | b :: rs -> expandFunApp (FunApp (f, [b]), rs)
    | [] -> failwith "eFA"
  in
  let uc1_let env expr = (* preserve multi-param *)
    match expr with
    | Lambda (ids, exp) -> 
      let env' = L.fold_left (fun a b -> (b, 1) :: a) env ids in
      Lambda (ids, uncurry1 env' exp)
    | _ -> uncurry1 env expr
  in
  match expr with
  | IntConst _ | FltConst _ | BoolConst _ | StrConst _ | Identifier _ -> expr

  | FunApp (s, lst) ->
    let lst = L.map (uncurry1 env) lst in (
    match uncurry1 env s with
    | Identifier id 
      when ((find id) = (L.length lst)) && (find id > 1) && (not_primitive id)
      -> FunApp (Identifier (id ^ "$"), lst)
    | Identifier id when not (not_primitive id) ->
      FunApp (Identifier id, lst)
    | ot -> expandFunApp (ot, lst))

  | Lambda (ids, exp) ->
    let env' = L.fold_left (fun a b -> (b, 1) :: a) env ids in
    List.fold_right (fun id e -> Lambda ([id], e)) ids (uncurry1 env' exp)

  | Cond lst -> Cond (L.map (fun (x, y) -> (uncurry1 env x, uncurry1 env y)) lst)

  | LetRec (lst, body) ->
    let env' = 
      L.fold_left 
        (fun env (id, exp) ->
           match exp with
           | Lambda (lst, _) -> (id, L.length lst) :: env
           | _ -> env)
        env lst
    in
    let bnds = 
      L.concat @@
      L.map (fun (i, e) -> expandBnd (i, uc1_let env' e)) lst
    in LetRec (bnds, uncurry1 env' body)

  | Let (lst, body) -> (* treating as nested *)
    let lst', env' = 
      L.fold_left 
        (fun (bnds, env) (id, exp) ->
           let env' = 
             match exp with
             | Lambda (lst, _) -> (id, L.length lst) :: env
             | _ -> env
           in (bnds @ (expandBnd (id, uc1_let env exp)), env'))
        ([], env) lst
    in
    Let (lst', uncurry1 env' body)

  | Tuple lst -> Tuple (L.map (uncurry1 env) lst)
  | Seq lst -> Seq (L.map (uncurry1 env) lst)
 

let uncurry e = 
  let e0 = uncurry0 Env.paramcnt e in
  uncurry1 Env.paramcnt e0



