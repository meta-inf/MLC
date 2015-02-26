open Typei
open Env
    
let infer astlist = 
  let te, ve = ref TEnv.init, VEnv.empty () in
  List.map (fun e -> Typei.infer e te ve) astlist 
