(letrec
 ((fact 
   (fun (x) 
	(cond 
	 ((== x 0) 1)
	 (#t (* x (fact (- x 1))))))))
 (dispi (fact 10)))
