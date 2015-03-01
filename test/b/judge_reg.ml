
  
let tests = [|


"(letrec ((f (fun (x) x))) (f 5))";

"((fun (x) (fun (y) (tuple x y 2))) 3 5)";

"(letrec ((f (fun (x) (fun (y) (tuple (+ x 2) (+ y 3)))))) (f 2 3))";

"(disp \"Hello, world!\")";

"(letrec ((fact (fun (x) (cond ((== x 0) 1) (#t (* x (fact (- x 1)))))))) (dispi (fact 10)))"
|];;(*
"(let ((v (ref 0))) (deref v))";

"((+ (* (/ 1 (- 1 2)) 3)) 4)";

"((fun (x) (x 2)) (fun (y) (+ y 2)))";


"(letrec ((fact (fun (x) (fun (y) (cond ((= x 0) y) (#t (fact (- x 1) (* y x)))))))) (fact 100000 1))";

"((fun (x) (+ x 2)) (* (+ (cond (#f (+ 2 3)) (#t (* 3 5))) 5) 6))"; (* cond with cont *)

"(+ (- (* (/ (/ (+ (- (* (/ (+ (- (* (/ (+ (- (* (/ (+ (- (* (/ 1 2) 3) 4) 5) 6) 7) 8) 9) 10) 11) 12) 13) 14) 15) 16) 17) 18) 19) 20) 21) 22)" (* register spilling *)

|];;

*)
