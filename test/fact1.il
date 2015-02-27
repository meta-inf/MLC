(let ((_ (letrec ((f (fun (x) (fun (y) (cond (((== x) 0) y) (#t ((f ((- x) 1)) ((* x) y)))))))) (dispi ((f 100000) 1))))) 0)
