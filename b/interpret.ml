(* source-level interpreter *)
open Afx
open Ast
open Interp
open Interp.Value
open Interp.Test
open Parse
open Unix

let test_r0 str = Eval.interp (Parse_debug.parse1 str)

let () =
  let prog = readAll () in
  let ctime = (Unix.times ()).tms_utime in
  let _ = test_r0 prog in
  let ctime' = (Unix.times ()).tms_utime in
  Printf.printf "\nExn time = @%.3f\n" (ctime' -. ctime)
