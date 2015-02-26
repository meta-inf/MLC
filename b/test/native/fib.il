(letrec ((fib (fun (x) (cond ((== x 0) 1) ((== x 1) 1) (#t (+ (fib (- x 1)) (fib (- x 2)))))))) (dispi (fib 27)))
