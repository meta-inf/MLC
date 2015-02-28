open Ast
open Afx
open Types

(* convert ast *)

let gensym =
  let s = ref 0 in
  fun () -> 
    begin
      s := !s + 1;
      Printf.sprintf "gs/%d" !s
    end

let rec convert_p exp =
  match exp with
  | IntConst _ | FltConst _ | StrConst _ | Var _ -> exp
  | FunApp (f, v) -> FunApp (convert_p f, convert_p v)
  | IfExp (a, b, Some c) -> 
    IfExp (convert_p a, convert_p b, Some (convert_p c))
  | IfExp (a, b, None) -> 
    IfExp (convert_p a, convert_p b, None)
  | Tuple lst -> Tuple (List.map convert_p lst)
  | AlType (s, lst) -> AlType (s, List.map convert_p lst)
  | Grouped lst -> Grouped (List.map convert_p lst)

  | MatchExp (e, lst) ->
    MatchExp (convert_p e,
              List.map (fun (p, e) -> (p, convert_p e)) lst)

  | Func (args, k) -> 
    let args' = 
      List.map 
        (function
          | PVar s -> (s, PVar s)
          | ot -> (gensym (), ot))
        args
    in
    let k' =
      List.fold_right 
        (fun (s, p) ret -> match p with
           | PVar s -> ret
           | _ -> MatchExp (Var s, [p, ret]))
        args'
        @@ convert_p k
    in Func (List.map (fun (s, _) -> PVar s) args', k')

  | Let (lbl, lst, k) -> 
    Let (lbl, 
         List.map 
           (fun (s, args, k) -> 
              if args = [] then (s, [], convert_p k)
              else (s, [], convert_p (Func (args, k))))
           lst,
         convert_p k)


let rec convert_f exp =
  match exp with
  | IntConst _ | FltConst _ | StrConst _ | Var _ -> exp
  | FunApp (f, v) -> FunApp (convert_f f, convert_f v)
  | IfExp (a, b, Some c) -> 
    IfExp (convert_f a, convert_f b, Some (convert_f c))
  | IfExp (a, b, None) -> 
    IfExp (convert_f a, convert_f b, None)
  | Tuple lst -> Tuple (List.map convert_f lst)
  | AlType (s, lst) -> AlType (s, List.map convert_f lst)
  | Grouped lst -> Grouped (List.map convert_f lst)

  | MatchExp (e, lst) ->
    MatchExp (convert_f e,
              List.map (fun (p, e) -> (p, convert_f e)) lst)

  | Func (arg :: [], k) -> (
      match arg with
      | PVar s -> Func ([arg], convert_f k)
      | _ -> failwith "convert_f")

  | Func (arg :: rel, k) ->
    convert_f @@ Func ([arg], Func (rel, k))

  | Let (lbl, lst, k) -> 
    Let (lbl, 
         List.map 
           (fun (s, args, k) -> 
              if args = [] then (s, [], convert_f k)
              else failwith "convert_f1")
           lst,
         convert_f k)

  | _ -> raise Not_implemented

let convertP stmt =
  match stmt with
  | GLetExp exp -> GLetExp (convert_p exp)
  | Expr exp -> Expr (convert_p exp)
  | _ -> stmt

let convertF stmt =
  match convertP stmt with
  | GLetExp exp -> GLetExp (convert_f exp)
  | Expr exp -> Expr (convert_f exp)
  | _ -> stmt
