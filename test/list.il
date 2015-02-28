(letrec 
((combine (fun (l1)
(fun (l2)
(let 
((mat/1 (tuple 0 l1 l2)))
(let 
((m/7a/0 (fun (m/d)
(tuple 16)))
(m/7a/1 (fun (a1 l1 a2 l2)
(tuple 17 (tuple 0 a1 a2) ((combine l1) l2)))))
(let 
((m/1/2 ((tuple-sel 2) mat/1)))
(let 
((m/1/20 ((tuple-sel 0) m/1/2)))
(cond
(((== 16) m/1/20)
(let 
((m/1/1 ((tuple-sel 1) mat/1)))
(let 
((m/1/10 ((tuple-sel 0) m/1/1)))
(cond
(((== 16) m/1/10)
(m/7a/0 0))
(#t
(match_failure 7))))))
(((== 17) m/1/20)
(let 
((m/1/1 ((tuple-sel 1) mat/1)))
(let 
((m/1/10 ((tuple-sel 0) m/1/1)))
(cond
(((== 17) m/1/10)
(let 
((m/1/11 ((tuple-sel 1) m/1/1)))
(let 
((m/1/12 ((tuple-sel 2) m/1/1)))
(let 
((m/1/21 ((tuple-sel 1) m/1/2)))
(let 
((m/1/22 ((tuple-sel 2) m/1/2)))
((((m/7a/1 m/1/11) m/1/12) m/1/21) m/1/22))))))
(#t
(match_failure 7))))))
(#t
(match_failure 7)))))))))))
(let 
((split (letrec 
((trav (fun (lst)
(fun (gs/9)
(let 
((m/6a/0 (fun (r1 r2)
(let 
((m/5a/0 (fun (m/d)
(tuple 0 r1 r2)))
(m/5a/1 (fun (a b rst)
((trav rst) (tuple 0 (tuple 17 a r1) (tuple 17 b r2))))))
(let 
((m/1/0 ((tuple-sel 0) lst)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) lst)))
(let 
((m/1/11 ((tuple-sel 1) m/1/1)))
(let 
((m/1/12 ((tuple-sel 2) m/1/1)))
(let 
((m/1/2 ((tuple-sel 2) lst)))
(((m/5a/1 m/1/11) m/1/12) m/1/2))))))
(((== 16) m/1/0)
(m/5a/0 0))
(#t
(match_failure 5))))))))
(let 
((m/1/1 ((tuple-sel 1) gs/9)))
(let 
((m/1/2 ((tuple-sel 2) gs/9)))
((m/6a/0 m/1/1) m/1/2))))))))
(fun (x)
((trav x) (tuple 0 (tuple 16) (tuple 16)))))))
(letrec 
((map (fun (f)
(fun (a)
(let 
((m/4a/0 (fun (m/d)
(tuple 16)))
(m/4a/1 (fun (x rs)
(tuple 17 (f x) ((map f) rs)))))
(let 
((m/1/0 ((tuple-sel 0) a)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) a)))
(let 
((m/1/2 ((tuple-sel 2) a)))
((m/4a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/4a/0 0))
(#t
(match_failure 4)))))))))
(letrec 
((length (fun (x)
(let 
((m/3a/0 (fun (m/d)
0))
(m/3a/1 (fun (a rs)
((+ 1) (length rs)))))
(let 
((m/1/0 ((tuple-sel 0) x)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) x)))
(let 
((m/1/2 ((tuple-sel 2) x)))
((m/3a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/3a/0 0))
(#t
(match_failure 3))))))))
(let 
((showlst (fun (l)
((map (fun (x)
(dispi x))) l))))
(let 
((fst (fun (gs/10)
(let 
((m/2a/0 (fun (a b)
a)))
(let 
((m/1/1 ((tuple-sel 1) gs/10)))
(let 
((m/1/2 ((tuple-sel 2) gs/10)))
((m/2a/0 m/1/1) m/1/2)))))))
(let 
((snd (fun (gs/11)
(let 
((m/1a/0 (fun (a b)
b)))
(let 
((m/1/1 ((tuple-sel 1) gs/11)))
(let 
((m/1/2 ((tuple-sel 2) gs/11)))
((m/1a/0 m/1/1) m/1/2)))))))
(let 
((_ (dispi (length (tuple 17 2 (tuple 16))))))
(let 
((_ (showlst (fst (split ((combine (tuple 17 2 (tuple 17 4 (tuple 16)))) (tuple 17 1 (tuple 17 3 (tuple 16)))))))))
0)))))))))
