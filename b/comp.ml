(* assembly compiler *)
open Afx
open Parse
open Interp
open Native

let () =
  let s = readAll () in
  (
    let r = (Reduce0.reduceBinOp @@ Parse_debug.parse1 s) in
    let (r, s, t, u, t0, t1, ret) = Minicomp.compileWithIR r in
    List.iter (Printf.printf "%s\n") ret
  )
