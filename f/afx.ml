module IntMap = Map.Make(struct type t = int let compare = compare end)
module StrMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)
module L = List

let hash = Hashtbl.hash

let flip f x y = f y x
let id x = x
  
let sprintf = Printf.sprintf

let set_union a b = L.filter (fun x -> L.mem x a) b 

exception Not_implemented
