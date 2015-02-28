module IntMap = Map.Make(struct type t = int let compare = compare end)
module StrMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

let readAll = 
  let rec read0 ret = 
    try let u = read_line () in read0 (u :: ret)
    with End_of_file -> ret
  in fun () -> String.concat "\n" (List.rev (read0 []))

let flip f x y = f y x
let id x = x

let list_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
