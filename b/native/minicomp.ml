let compileWithIR (r: Ast.expr) =
  let r = Prep.rename r in (* rename nested let *)
  let s = Cps.trans r in (* -> dexpr *)
  let t = Reduce.reduce s in (* -> cexpr *)
  let (_, u) = Closure.buildClosure t in 
  let t0 = Prereg.flatten u in (* -> int Prog.t *)
  let t1 = Register.registerize t0 in (* -> Register.RM.t Prog.t *)
  let lst = Mgen.mgen t1 in (* -> string list *)
  (r, s, t, u, t0, t1, lst)

let compile (r: Ast.expr) =
  let (_, _, _, _, _, _, ret) = compileWithIR r
  in ret

