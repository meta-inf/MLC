  
let tests = [|

"(fun (x) x)";

"(fun (x) (fun (x) x))";

"(fun (x) (fun (y) (tuple x y 2)))";

"(let ((x (fun (x) x)) (y (fun (x) x))) (fun (x) (fun (x) (tuple x y))))";

"(letrec ((x (fun (x) x)) (y (fun (x) y))) (fun (z) (fun (z) (tuple x y z))))";

"((+ (* (/ 1 (- 1 2)) 3)) 4)";

"(+ (- (* (/ (/ (+ (- (* (/ (+ (- (* (/ (+ (- (* (/ (+ (- (* (/ 1 2) 3) 4) 5) 6) 7) 8) 9) 10) 11) 12) 13) 14) 15) 16) 17) 18) 19) 20) 21) 22)"; (* register spilling *)

"(((fun (x) (fun (y) (+ x y))) 1) 2)";

"((fun (x) (x 2)) (fun (y) (+ y 2)))";

"(letrec ((fact (fun (x) (cond ((= x 0) 1) (#t (* x (fact (- x 1)))))))) (fact 10))";

"(letrec ((fact (fun (x) (fun (y) (cond ((= x 0) y) (#t (fact (- x 1) (* y x)))))))) (fact 100000 1))";

"(letrec ((fib (fun (x) (cond ((= x 0) 1) ((= x 1) 1) (#t (+ (fib (- x 1)) (fib (- x 2)))))))) (fib 17))";

"(letrec ((even (fun (x) (cond ((= x 0) #t) (#t (odd (- x 1)))))) (odd (fun (x) (cond ((= x 0) #f) ((= x 1) #t) (#t (even (- x 1))))))) (tuple (even 0) (odd 3) (even 5) (even 6) (odd 8)))";

"(let ((v (ref 0))) (deref v))";

"(let 
 ((acc (let 
		((v (ref 0))) 
		(let ((acc (fun (x) 
					(let 
					 ((dum (:= v (+ (deref v) 1)) ))
					 (deref v)
					 ))))
		 acc))))
 (tuple (acc 0) (acc 0)))";

"((fun (x) (+ x 2)) (* (+ (cond (#f (+ 2 3)) (#t (* 3 5))) 5) 6))"

|];;


