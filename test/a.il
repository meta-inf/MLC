(letrec ((f (fun (x) (cond (((= x) 0) 1) (#t ((* (f ((- x) 1))) x)))))) (let ((_ (dispi (f 10)))) 0))
