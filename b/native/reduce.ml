(* reduce.ml: constant (and variable) folding in cps *)

open Afx
open Cps

let reduce = 
  let m2i v = match v with
    | Int a -> a
    | _ -> failwith ""
  and m2b v = match v with
    | Bool b -> b
    | _ -> failwith ""
  and is_const = function
    | Var _ | ClosureVal _ -> false
    | Int _ | Float _ | Bool _ | Str _ | Unit -> true

  and find (ID i) env = try IntMap.find i env with Not_found -> Var (ID i)
  and add (ID i) v env = IntMap.add i v env
  in
  
  let rec reduceP1 i v k env =
    match snd @@ Parse.IdTable.str_of_id i with
    | "not" -> reduce0 (DValue (Bool (not (m2b v)), k)) env
    | _ -> CPrimOp1 (i, v, reduce k env)

  and reduceP2 i a b k env =
    match snd @@ Parse.IdTable.str_of_id i with
    | "+" -> reduce0 (DValue (Int ((m2i a) + (m2i b)), k)) env
    | "-" -> reduce0 (DValue (Int ((m2i a) - (m2i b)), k)) env
    | "*" -> reduce0 (DValue (Int ((m2i a) * (m2i b)), k)) env
    | "/" -> reduce0 (DValue (Int ((m2i a) / (m2i b)), k)) env
    | "or" -> reduce0 (DValue (Bool ((m2b a) || (m2b b)), k)) env
    | "=" -> reduce0 (DValue (Bool (a = b), k)) env
    | "==" -> 
      let ret = match a with
        | Str _ -> false
        | _ -> a = b
      in reduce0 (DValue (Bool ret, k)) env
    | _ -> CPrimOp2 (i, a, b, reduce k env)

  and reduce dexp env = 
    match dexp with
    | DExp (i, impl) -> CExp (i, reduce0 impl env)

  and reduce0 exp env = (* variables should form a DSU *)
    let find i = find i env in
    match exp with
    | DTerm -> CTerm
    | DValue (Var i, DExp (j, exp')) -> reduce0 exp' (add j (find i) env)
    | DValue (v, DExp (i, exp')) -> reduce0 exp' (add i v env)
    | DPrimOp1 (i, v, k) -> 
      let nv = find v in
      if is_const nv then reduceP1 i nv k env
      else CPrimOp1 (i, nv, reduce k env)
    | DPrimOp2 (i, a, b, k) ->
      let (na, nb) = (find a, find b) in
      if (is_const na) && (is_const nb) then reduceP2 i na nb k env
      else CPrimOp2 (i, na, nb, reduce k env)
    | DCond (i, k1, k2) -> (
      match find i with
      | Bool v -> if v then reduce0 k1 env else reduce0 k2 env
      | ot -> CCond (ot, reduce0 k1 env, reduce0 k2 env))
    | DFunApp (i, j) -> CFunApp (find i, List.map find j)
    | DTuple (lst, k) -> CTuple (List.map find lst, reduce k env)
    | DFix (lst, kimpl) -> 
      CFix (List.map 
              (fun (id, args, i) -> (id, args, reduce0 i env)) lst,
            [||], 
            reduce0 kimpl env)

  in fun e -> reduce0 e IntMap.empty
