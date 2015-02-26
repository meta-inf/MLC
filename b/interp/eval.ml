(* eval.ml: source-level continuation-passing interpreter *)

open Ast
open Value

let rec condCont lst env refs cont =
  match lst with
  | [] -> failwith "Empty cond"
  | (_, brTrue) :: [] ->
    (fun tval ->
       if v2b tval then (eval brTrue env refs cont)
       (* if and match in ML will not translate into this case *)
       else failwith "cond: non-exhaustive cond") 
  | (_, brTrue) :: ret ->
    (fun tval ->
       if v2b tval then (eval brTrue env refs cont)
       else eval (fst @@ List.hd ret) env refs (condCont ret env refs cont))

and funAppCont arg env refs cont =
  function
  | BuiltinFunc impl -> 
    (eval arg env refs (fun a -> cont (impl a)))
  | BuiltinFuncS impl -> 
    (eval arg env refs (fun a -> cont (impl a refs)))
  | Func (id, exp, env) -> 
    (eval arg env refs (fun a -> eval exp (Table.add id a env) refs cont))
  | _ -> failwith "fun-app: not a function"

and letRecCont lst body env refs cont =
  match lst with
  | [] -> failwith "letrec - empty binding"
  | (id, _) :: [] ->
    (* letrec (... and) id = _ (value) in body
     * add id->value to valtable, then evaluate body *)
    (fun value -> eval body (Table.add_r id value env) refs cont)
  | (id, _) :: (((_, valexpr') :: _) as ret) ->
    (* letrec id = _ (value) (and id' = valexpr' ...) in ...
     * add (id,value) to valtable -> evaluate valexpr' -> process rest bindings
     * (in the new table!) *)
    (fun value ->
       let env' = Table.add_r id value env in
       eval valexpr' env' refs (letRecCont ret body env' refs cont))

and letCont lst body kvpairs env refs cont =
  match lst with
  | [] -> failwith "let - empty binding"
  | (id, _) :: [] -> (* roll up envtable and go to body *)
    (fun value -> 
       let env' = List.fold_left
           (fun curenv kvpar -> Table.add (fst kvpar) (snd kvpar) curenv)
           (Table.add id value env) kvpairs
       in eval body env' refs cont)
  | (id, _) :: (((_, valexpr') :: _) as ret) ->
    (fun value -> 
       eval valexpr' env refs (letCont ret body ((id, value) :: kvpairs) env refs cont))

and tupleCont exprlst valuelst env refs cont =
  match exprlst with (* 我以为... 实际上... *)
  | [] -> failwith "tuple - empty lst"
  | _ :: [] -> (* submit valuelst to cont *)
    (fun value -> cont (Tuple (Array.of_list @@ valuelst @ [value])))
  | _ :: ((valexpr' :: _) as ret) -> (* continue evaluation *)
    (fun value ->
       eval valexpr' env refs (tupleCont ret (valuelst @ [value]) env refs cont))

and eval expr env refs cont =
  match expr with
  | IntConst v -> cont (IntVal v)
  | FltConst v -> cont (FltVal v)
  | BoolConst v -> cont (BoolVal v)
  | StrConst v -> cont (StrVal v)
  | Identifier i -> cont (Table.find i env)
  | Lambda ([id], body) -> cont (Func (id, body, env))
  | Lambda _ -> failwith "eval"
  | Cond lst -> eval (fst @@ List.hd lst) env refs (condCont lst env refs cont)
  | FunApp (func, arg) -> eval func env refs (funAppCont arg env refs cont)
  | FunApp1 (id, arg) -> (
    match Reduce0.builtinFunc id with
    | BuiltinFunc f -> 
      eval arg env refs (fun va -> cont (f va))
    | BuiltinFuncS f ->
      eval arg env refs (fun va -> cont (f va refs))
    | _ -> failwith "...")
  | FunApp2 (id, el, er) -> (
    match Reduce0.binOpFunc id with
    | PureBF f -> 
      eval el env refs (fun vl -> eval er env refs (fun vr -> cont (f vl vr)))
    | ImpureBF f ->
      eval el env refs (fun vl -> eval er env refs (fun vr -> cont (f vl vr refs)))
    )
  (* letrec: will raise "identifier-not-found" on `letrec a = a + 5`. *)
  | LetRec (lst, body) ->
    eval (snd @@ List.hd lst) env refs (letRecCont lst body env refs cont)
  | Let (lst, body) ->
    eval (snd @@ List.hd lst) env refs (letCont lst body [] env refs cont)
  | Tuple lst ->
    eval (List.hd lst) env refs (tupleCont lst [] env refs cont)
;;

let rec simplify e = 
  match e with
  | IntConst _ | FltConst _ | BoolConst _ | StrConst _ | Identifier _ -> e
  | Cond lst -> Cond (List.map (fun (a, b) -> (simplify a, simplify b)) lst)
  | Lambda (lst, e) -> 
    List.fold_right (fun i e -> Lambda ([i], e)) lst (simplify e) 
  | FunApp (s, e) -> FunApp (simplify s, simplify e)
  | FunApp1 (i, e) -> FunApp1 (i, simplify e)
  | FunApp2 (i, a, b) -> FunApp2 (i, simplify a, simplify b)
  | Let (lst, k) -> 
    Let (List.map (fun (i, e) -> (i, simplify e)) lst, simplify k)
  | LetRec (lst, k) -> 
    LetRec (List.map (fun (i, e) -> (i, simplify e)) lst, simplify k)
  | Tuple lst -> Tuple (List.map simplify lst)

let interp (e: expr) = 
  eval (simplify e) Table.empty (ref RefTable.empty) (fun x -> x)
