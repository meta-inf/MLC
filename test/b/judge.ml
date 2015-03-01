(* Test driver. *)

open Afx
open Ast
open Interp
open Interp.Value
open Interp.Test
open Parse
open Unix
open Native
open Native.Cps
open Native.Register

let termExp ct = 
    let i = Cps.C.get ct in
    Cps.DExp (i, Cps.DTerm)

let dprint = false

let test_r0 str = 
  let r = (Reduce0.reduceBinOp @@ Parse_debug.parse1 str) in
  let (r, s, t, u, t0, t1, ret) = Minicomp.compileWithIR r in
  (str, t0, t1, ret)

let output = List.iter (Printf.printf "%s\n")

let () = 
  Array.iter (fun (s, _, _, u) -> 
      Printf.printf "%s:\n\n" s; output u; Printf.printf "\n\n")
  @@ Array.map test_r0 Judge_reg.tests
