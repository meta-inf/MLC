(let 
 ((v (ref 0)))
 (let ((u (:= v (+ 1 (deref v))))) (dispi (deref v))))
