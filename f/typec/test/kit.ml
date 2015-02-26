open TagPrinter
open Typei
open Parse.Parser
open Infer

let mexp sigstr = 
  function
  | TRExp si -> sigstr = (printTag si)
  | _ -> false

let mlet siglst =
  function
  | TRLet ls -> 
    let ls0 = List.map (fun (s, i) -> (s, printTag i)) ls in
    ls0 = siglst
  | _ -> false

let mnone = function
  | TRNone -> true
  | _ -> false

let run_test tests ans =
  let astlst = parse1 tests in
  try
    let faillst = ref [] in
    let res = infer astlst in
    let sts = 
      List.mapi 
        (fun i (a, b) -> 
           if a b then true
           else (faillst := !faillst @ [i]; false))
        (List.combine ans res)
    in
    if List.for_all (fun x -> x) sts then 
      ("Passed", []) 
    else ("FAILED", !faillst)
  with 
    Not_found -> ("CRASHED", [])
  | Type_error _ -> ("FAILED_c", [])
