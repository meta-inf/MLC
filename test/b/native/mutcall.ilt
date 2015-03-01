(letrec
 ((even (fun (x) (cond
		  ((== x 0) #t)
		  ((== x 1) #f)
		  (#t (odd (- x 1))))))
  (odd (fun (x) (cond
		 ((== x 0) #f)
		 ((== x 1) #t)
		 (#t (even (- x 1)))))))
 (dispi (even 10)))
