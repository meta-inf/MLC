(letrec 
((map (fun (f)
(fun (a)
(let 
((m/5a/0 (fun (m/d)
(tuple 16)))
(m/5a/1 (fun (x rs)
(tuple 17 (f x) ((map f) rs)))))
(let 
((m/1/0 ((tuple-sel 0) a)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) a)))
(let 
((m/1/2 ((tuple-sel 2) a)))
((m/5a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/5a/0 0))
(#t
(match_failure 5)))))))))
(let 
((showlst (fun (l)
((map (fun (x)
(dispi x))) l))))
(let 
((_ (showlst (tuple 17 1 (tuple 17 2 (tuple 16))))))
(letrec 
((merge (fun (cmp)
(fun (l1)
(fun (l2)
(let 
((mat/1 (tuple 0 l1 l2)))
(let 
((m/4a/0 (fun (l2)
l2))
(m/4a/1 (fun (l1)
l1))
(m/4a/2 (fun (h1 t1 h2 t2)
(cond
(((< ((cmp h1) h2)) 0)
(tuple 17 h1 (((merge cmp) t1) l2)))
(#t
(tuple 17 h2 (((merge cmp) l1) t2)))))))
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
(((== 17) m/1/10)
(m/4a/1 m/1/1))
(((== 16) m/1/10)
(m/4a/0 m/1/2))
(#t
(m/4a/1 m/1/1))))))
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
((((m/4a/2 m/1/11) m/1/12) m/1/21) m/1/22))))))
(((== 16) m/1/10)
(m/4a/0 m/1/2))
(#t
(match_failure 4))))))
(#t
(let 
((m/1/1 ((tuple-sel 1) mat/1)))
(let 
((m/1/10 ((tuple-sel 0) m/1/1)))
(cond
(((== 16) m/1/10)
(m/4a/0 m/1/2))
(#t
(match_failure 4))))))))))))))))
(let 
((_ (showlst (((merge (fun (x)
(fun (y)
((- x) y)))) (tuple 17 1 (tuple 17 3 (tuple 16)))) (tuple 17 2 (tuple 16))))))
(letrec 
((rev_append (fun (l1)
(fun (l2)
(let 
((m/3a/0 (fun (m/d)
l2))
(m/3a/1 (fun (a l)
((rev_append l) (tuple 17 a l2)))))
(let 
((m/1/0 ((tuple-sel 0) l1)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) l1)))
(let 
((m/1/2 ((tuple-sel 2) l1)))
((m/3a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/3a/0 0))
(#t
(match_failure 3)))))))))
(let 
((rev (fun (l)
((rev_append l) (tuple 16)))))
(let 
((find_all (fun (p)
(letrec 
((find (fun (accu)
(fun (x)
(let 
((m/2a/0 (fun (m/d)
(rev accu)))
(m/2a/1 (fun (x l)
(cond
((p x)
((find (tuple 17 x accu)) l))
(#t
((find accu) l))))))
(let 
((m/1/0 ((tuple-sel 0) x)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) x)))
(let 
((m/1/2 ((tuple-sel 2) x)))
((m/2a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/2a/0 0))
(#t
(match_failure 2)))))))))
(find (tuple 16))))))
(let 
((filter find_all))
(let 
((concat (fun (a)
(fun (b)
((rev_append (rev a)) b)))))
(let 
((qsort (fun (lis)
(letrec 
((traverse (fun (l)
(let 
((m/1a/0 (fun (m/d)
(tuple 16)))
(m/1a/1 (fun (x xs)
((concat (traverse ((filter (fun (y)
((< y) x))) xs))) (tuple 17 x (traverse ((filter (fun (y)
((< x) y))) xs)))))))
(let 
((m/1/0 ((tuple-sel 0) l)))
(cond
(((== 17) m/1/0)
(let 
((m/1/1 ((tuple-sel 1) l)))
(let 
((m/1/2 ((tuple-sel 2) l)))
((m/1a/1 m/1/1) m/1/2))))
(((== 16) m/1/0)
(m/1a/0 0))
(#t
(match_failure 1))))))))
(traverse lis)))))
(let 
((_ (let 
((a (qsort (tuple 17 1 (tuple 17 3 (tuple 17 2 (tuple 17 5 (tuple 17 4 (tuple 16)))))))))
(showlst a))))
0))))))))))))
