open Parse
open Typec
open Il

let compile s = 
  let t = Parser.parse1 s in
  let _ = Infer.infer t in
  let r = List.map Pretype.convertP t in
  let u = Translate.translate r in
  u
  
let () =
  let s = Parser.readAll () in
  let u = compile s in
  IAst.printAst u
