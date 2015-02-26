(* Test driver. *)

open Afx
open Ast
open Interp
open Interp.Value
open Interp.Test
open Parse
open Unix
open Trans
open Trans.Cps
open Trans.Register

let termExp ct = 
    let i = Cps.C.get ct in
    Cps.DExp (i, Cps.DTerm)

let dprint = false

let test_r0 str = 
  let r = Prep.rename (Reduce0.reduceBinOp @@ Parse_debug.parse1 str) in
  let s = Cps.trans r in
  let t = Reduce.reduce s in
  let u = Closure.buildClosure t in
  begin
   (* Printerc.p str; Printerc.p "\n";
    Printerc.print t;
    Printerc.p "\n\n";*)
    if dprint then (
      Printerc.print (snd u);
      Printerc.p "\n";
      if (Closure.M.cardinal (fst u) != 0) then 
        begin
          Printerc.p "failed: ";
          Array.iter (fun (Cps.ID x) -> Printerc.p (Printerc.soi x)) (Closure.M.to_arr @@ fst u);
          Printerc.p "\n\n"
        end
    );
    let (lst, main) = registerize @@ snd u in
    begin
      if dprint then (
        List.iter (fun (id, cnt, x) -> begin
              Printf.printf "fix %s = (%d vars) \n" (Printerc.soi id) cnt;
              Printerc.printR x
            end) lst;
        Printf.printf "main = \n";
        Printerc.printR main;
        Printerc.p "\n\n");
      (u, lst, main)
    end
  end

let output = List.iter (Printf.printf "%s\n")

let () = 
  Array.iter (fun (s, (_, a, b)) -> 
      let u = Mgen.mgen (a, b)
      in (Printf.printf "%s:\n\n" s; output u; Printf.printf "\n\n"))
    @@ Array.map (function str -> (str, test_r0 str)) Judge_reg.tests
