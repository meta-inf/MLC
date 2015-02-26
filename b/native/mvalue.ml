open Afx

type mvalue = StkVar of int
            | CloVar of int
            | Reg of int
            | RegF of int
            | CloRef of mvalue * mvalue option * int
            (* (closure-data, closure, funcID) *) 
            | Imm of int
            | ImmS of int (* label of string *)
            | ImmF of float
            | Label of int (* label of function *)

type freqmap = int IntMap.t

type 'a mexp = MPrimOp1 of 'a * int * mvalue * mvalue * 'a mexp
             | MPrimOp2 of 'a * int * mvalue * mvalue * mvalue * 'a mexp
             | MCond of 'a * mvalue * 'a mexp * 'a mexp
             | MFunApp of 'a * mvalue * (mvalue list)
             | MTuple of 'a * mvalue * (mvalue list) * 'a mexp
             | MCopy of 'a * mvalue * mvalue * 'a mexp
             | MTerm

type 'a mfixExp = Proc of int * int * int * int * ('a mexp) (* id, tempvar used, heapmem used, argcnt, impl *)
                | Main of int * int * ('a mexp)

module MVMap = Map.Make (struct
    type t = mvalue
    let compare v1 v2 = compare (Hashtbl.hash v1) (Hashtbl.hash v2)
  end)

module Prog =
struct
  type 'a t = int StrMap.t * ('a mfixExp) list

  let empty () = (StrMap.empty, [])
  let addStr (s: string) (r: 'a t ref) =
    let (sm, lst) = !r in
    try StrMap.find s sm
    with Not_found -> 
      let cnt = StrMap.cardinal sm in
      (r := (StrMap.add s cnt sm, lst); cnt)

  let addProc (f: 'a mfixExp) (r: 'a t ref) =
    let (sm, lst) = !r in r := (sm, f :: lst)
end

let varMatch f1 f2 =
  fun mval -> 
    match mval with
    | (StkVar _ | CloVar _ | CloRef _) -> f1 mval
    | _ -> f2 mval

let varMatchNC f1 f2 =
  fun mval -> 
    match mval with
    | (StkVar _ | CloVar _) -> f1 mval
    | CloRef _ -> raise (Invalid_argument "varMatchNC: CloRef not allowed")
    | _ -> f2 mval
