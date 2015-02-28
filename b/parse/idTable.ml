module IdMap = Map.Make(struct type t = string let compare = compare end)

let (tbl, tbl_orig) =
  let tbl_real = List.fold_left 
      (fun ev id -> IdMap.add id (IdMap.cardinal ev) ev)
      IdMap.empty
      ["=="; "="; "+"; "-"; "*"; "/"; ">"; "<"; "$";
       "not"; "or"; "ref"; ":="; "deref"; 
       "tuple-sel"; "id"; "disp"; "dispi"; "match_failure"]
  in (ref tbl_real, tbl_real)

let findOrAdd str =
  if IdMap.exists (fun a b -> a = str) (! tbl)
  then IdMap.find str !tbl
  else
    let ret = 1 + IdMap.cardinal !tbl
    in (tbl := IdMap.add str ret !tbl; ret)

let is_primitive id = IdMap.exists (fun a b -> b = id) tbl_orig

let find str = IdMap.find str !tbl

let str_of_id = 
  let lst = List.map (fun (x, y) -> (y, x)) @@ (IdMap.bindings tbl_orig) in
  fun id ->
    try (true, List.assoc id lst)
    with Not_found -> (false, "")

