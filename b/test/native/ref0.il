(let 
 ((v (ref 0)))
 (let ((u (set-ref v (+ 1 (deref v))))) (dispi (deref v))))
