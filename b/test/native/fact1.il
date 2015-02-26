(letrec ((fact (fun (x) (fun (y) (cond ((== x 0) y) (#t (fact (- x 1) (* y x)))))))) (dispi (fact 1000000 1)))
