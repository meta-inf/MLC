(letrec 
((combine (fun (l1)
(fun (l2)
(let 
((mat/1 (tuple 0 l1 l2)))
(let 
((m/8a/0 (fun (m/d)
(tuple 16)))
(m/8a/1 (fun (a1 l1 a2 l2)
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
(m/8a/0 0))
(#t
(match_failure 8))))))
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
((((m/8a/1 m/1/11) m/1/12) m/1/21) m/1/22))))))
(#t
(match_failure 8))))))
(#t
(match_failure 8)))))))))))
(letrec 
((rev_append (fun (l1)
(fun (l2)
(let 
((m/7a/0 (fun (m/d)
l2))
(m/7a/1 (fun (a l)
((rev_append l) (tuple 17 a l2)))))
(let 
((m/1/0 ((tuple-sel 0) l1)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) l1)))
(let 
((m/1/2 ((tuple-sel 2) l1)))
((m/7a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/7a/0 0))
(#t
(match_failure 7)))))))))
(let 
((rev (fun (l)
((rev_append l) (tuple 16)))))
(let 
((fst (fun (gs/10)
(let 
((m/6a/0 (fun (a b)
a)))
(let 
((m/1/1 ((tuple-sel 1) gs/10)))
(let 
((m/1/2 ((tuple-sel 2) gs/10)))
((m/6a/0 m/1/1) m/1/2)))))))
(let 
((snd (fun (gs/11)
(let 
((m/5a/0 (fun (a b)
b)))
(let 
((m/1/1 ((tuple-sel 1) gs/11)))
(let 
((m/1/2 ((tuple-sel 2) gs/11)))
((m/5a/0 m/1/1) m/1/2)))))))
(let 
((split (letrec 
((trav (fun (lst)
(fun (gs/12)
(let 
((m/4a/0 (fun (r1 r2)
(let 
((m/3a/0 (fun (m/d)
(tuple 0 r1 r2)))
(m/3a/1 (fun (a b rst)
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
(((m/3a/1 m/1/11) m/1/12) m/1/2))))))
(((== 16) m/1/0)
(m/3a/0 0))
(#t
(match_failure 3))))))))
(let 
((m/1/1 ((tuple-sel 1) gs/12)))
(let 
((m/1/2 ((tuple-sel 2) gs/12)))
((m/4a/0 m/1/1) m/1/2))))))))
(fun (x)
(let 
((r ((trav x) (tuple 0 (tuple 16) (tuple 16)))))
(tuple 0 (($ rev) (fst r)) (($ rev) (snd r))))))))
(letrec 
((map (fun (f)
(fun (a)
(let 
((m/2a/0 (fun (m/d)
(tuple 16)))
(m/2a/1 (fun (x rs)
(tuple 17 (f x) ((map f) rs)))))
(let 
((m/1/0 ((tuple-sel 0) a)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) a)))
(let 
((m/1/2 ((tuple-sel 2) a)))
((m/2a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/2a/0 0))
(#t
(match_failure 2)))))))))
(letrec 
((length (fun (x)
(let 
((m/1a/0 (fun (m/d)
0))
(m/1a/1 (fun (a rs)
((+ 1) (length rs)))))
(let 
((m/1/0 ((tuple-sel 0) x)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) x)))
(let 
((m/1/2 ((tuple-sel 2) x)))
((m/1a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/1a/0 0))
(#t
(match_failure 1))))))))
(let 
((showlst (fun (l)
(let 
((_ ((map (fun (x)
(let 
((_ (dispi x)))
(disp " ")))) l)))
(disp "\n")))))
(let 
((_ (($ showlst) (($ fst) (($ split) ((combine (tuple 17 2 (tuple 17 4 (tuple 16)))) (tuple 17 1 (tuple 17 3 (tuple 16)))))))))
(let 
((_ (($ showlst) (($ snd) (($ split) ((combine (tuple 17 2 (tuple 17 4 (tuple 16)))) (tuple 17 1 (tuple 17 3 (tuple 16)))))))))
0)))))))))))
