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

let rec convert_e exp =
  match exp with
  | IntConst _ | FltConst _ | Var _ -> exp
  | FunApp (f, v) -> FunApp (convert_e f, convert_e v)
  | IfExp (a, b, Some c) -> 
    IfExp (convert_e a, convert_e b, Some (convert_e c))
  | IfExp (a, b, None) -> 
    IfExp (convert_e a, convert_e b, None)
  | Tuple lst -> Tuple (List.map convert_e lst)
  | AlType (s, lst) -> AlType (s, List.map convert_e lst)
  | Grouped lst -> Grouped (List.map convert_e lst)

  | MatchExp (e, lst) ->
    MatchExp (convert_e e,
              List.map (fun (p, e) -> (p, convert_e e)) lst)

  | Func (arg :: [], k) -> (
    match arg with
    | PVar s -> Func ([arg], convert_e k)
    | _ -> 
      let sym = gensym () in
      Func ([PVar sym], MatchExp (Var sym, [arg, convert_e k])))
  | Func (arg :: rel, k) ->
    convert_e @@ Func ([arg], Func (rel, k))

  | Let (lbl, lst, k) -> 
    Let (lbl, 
         List.map 
           (fun (s, args, k) -> 
              if args = [] then (s, [], convert_e k)
              else (s, [], convert_e (Func (args, k))))
           lst,
         convert_e k)

  | _ -> raise Not_implemented

let convert stmt =
  match stmt with
  | GLetExp exp -> GLetExp (convert_e exp)
  | Expr exp -> Expr (convert_e exp)
  | _ -> stmt
