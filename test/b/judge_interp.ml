(* Test driver. *)

open Ast
open Interp
open Interp.Value
open Interp.Test
open Parse
open Unix

let test_r0 str = Eval.interp (Parse_debug.parse1 str)

let tests = 
  let eb v = fun x -> v2b x == v
  and ei v = fun x -> v2i x == v
  and eg v = fun x -> x = v
  in ["const0", "2", ei 2;
"const1", "(= 2 3)", eb false;
"cond0", "((fun (v1) (cond ((= v1 0) 2) ((= v1 1) 4) (#t 6))) 0)", ei 2;
"cond1", "((fun (v1) (cond ((= v1 0) 2) ((= v1 1) 4) (#t 6))) 1)", ei 4;
"cond2", "((fun (v1) (cond ((= v1 0) 2) ((= v1 1) 4) (#t 6))) 2)", ei 6;
"func0", "(((fun (v0) (fun (v2) ((+ v0) v2))) 3) 4)", ei 7;
"tuple", "(tuple-sel 0 (tuple #t 5 3))", eb true;
"letscp0", "(let ((v0 7) (v1 5)) (let ((v0 2) (v1 3)) (tuple v0 v1)))", eg (Tuple [|IntVal 2; IntVal 3|]);
"letscp1", "(let ((v0 7) (v1 5)) (letrec ((v0 2) (v1 3)) (tuple v0 v1)))", eg (Tuple [|IntVal 2; IntVal 3|]);
"letrc0", "(letrec ((fact (fun (x) (cond ((= x 0) 1) (#t (* x (fact (- x 1)))))))) (fact 10))", ei 3628800;
"letrc1", "(letrec ((fact (fun (x y) (cond ((= x 0) y) (#t (fact (- x 1) (* y x))))))) (fact 100000 1))", ei 0;
"letrc2", "(letrec ((fib (fun (x) (cond ((= x 0) 1) ((= x 1) 1) (#t (+ (fib (- x 1)) (fib (- x 2)))))))) (fib 17))", ei 2584;
"letrc3", 
"(letrec ((even (fun (x) (cond ((= x 0) #t) (#t (odd (- x 1)))))) (odd (fun (x) (cond ((= x 0) #f) ((= x 1) #t) (#t (even (- x 1))))))) (tuple (even 0) (odd 3) (even 5) (even 6) (odd 8)))", eg (Tuple [|BoolVal true; BoolVal true; BoolVal false; BoolVal true; BoolVal false|]);
"ref0", "(let ((v (ref 0))) (deref v))", ei 0;
"ref1", 
"(let 
 ((acc (let 
		((v (ref 0))) 
		(let ((acc (fun (x) 
					(let 
					 ((dum (set-ref v (+ (deref v) 1)) ))
					 (deref v)
					 ))))
		 acc))))
 (tuple (acc 0) (acc 0)))",
eg (Tuple [|IntVal 1; IntVal 2|])
     ];;

let pcnt = ref 0 and fcnt = ref 0

let () = begin
  ignore @@
    List.map (function (name, prog, passed) ->
      begin
        Printf.printf "Case %s: " name;
      (*  Parse.Printer.printAstT @@ Interp.Reduce0.reduceBinOp @@ Parse.Parse_debug.parse1 prog;
        Printf.printf " ";*)
        let ctime = (Unix.times ()).tms_utime in
        let ret = test_r0 prog in
        let ctime' = (Unix.times ()).tms_utime in
        if (passed ret) then
          begin
            pcnt := !pcnt + 1;
            Printf.printf "\tPassed\t@%.3f\n" (ctime' -. ctime)
          end
        else 
          begin
            fcnt := !fcnt + 1;
            Printf.printf "\tFAILED\t@%.3f\n" (ctime' -. ctime)
          end
      end) tests;
  if (!fcnt == 0) then Printf.printf "All test passed.\n" else
    Printf.printf "Passed %d; failed %d.\n" !pcnt !fcnt
end
