open Typec
open Afx
open Parse
open Ast
open Types
open Typei
open Parser
open Test
open Il
open IAst
open Patcomp
open TagPrinter;;

Test.Run.run ();;

Infer.infer @@ parse1 Variant.tests;;

let meoe [Ast.Expr (Ast.MatchExp (_, r))] = r

let pat b = L.mapi (fun i (a, _) -> (collectTrait a, i)) b 

let pool pat = poolOfTraits @@ markConflict pat

let check b = 
  IAst.printAst @@ Patcomp.translate "r" @@ List.mapi (fun i (x, y) -> (x, IAst.IntConst i)) b

let s = [|
  "match x with 0 -> 0 | 1 -> 1 | 2 -> 2;";
  "match x with [] -> [] | a :: b -> b;";
  "match x with [] -> [] | a :: [] -> [] | a :: b :: c -> c;";
  "
  match x with
  | (Cons (2, Cons (3, d, e), Nil), 3, f) -> 1
  | (Cons (2, Cons (4, Nil, a), Nil), b, 3) -> 2
  | (Nil, 3, 3) -> 3
  ;"|]

let check1 s = check @@ meoe @@ parse1 s;;
