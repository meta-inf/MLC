(let 
 ((acc (let
	((v (ref 0)))
	(fun (x) 
	 (let
	  ((dummy (:= v (+ 1 (deref v)))))
	  (deref v))))))
 (letrec
  ((test1 (fun (x)
	   (cond
	    ((== x 0) 0)
	    (#t (let 
		 ((dummy (dispi (acc 0))))
		 (test1 (- x 1))))))))
 (test1 10)))
