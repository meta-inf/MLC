type 'a tree = Nil
             | Node of ('a, 'a tree, 'a tree);

let show =
  let rec trav t = 
    match t with
    | Nil -> disp " "
    | Node (i, l, r) -> 
      begin
        disp "("; dispi i; disp ", ";
        trav l; disp ", ";
        trav r; disp ")"
      end
  in fun t ->
    begin
      trav t;
      disp "\n"
    end;

let rec insert t v =
  match t with
  | Nil -> Node (v, Nil, Nil)
  | Node (x, l, r) -> 
    if v < x then Node (x, insert l v, r)
    else Node (x, l, insert r v)
;

let rec splitLT t v =
  match t with
  | Nil -> Nil
  | Node (x, l, r) ->
    if x < v then Node (x, l, splitLT r v)
    else splitLT l v;

let rec splitGT t v =
  match t with
  | Nil -> Nil
  | Node (x, l, r) ->
    if x > v then Node (x, splitGT l v, r)
    else splitGT r v;

let rec merge l r =
  match (l, r) with
  | (a, Nil) | (Nil, a) -> a
  | (Node (x, a, b), y) -> Node (x, a, merge b y)
;

let rec fold_left f sum lst =
  match lst with
  | [] -> sum
  | a :: rs -> fold_left f (f sum a) rs
;

let from_list = fold_left insert Nil;
  
let t = from_list [6, 2, 3, 1, 5, 4, 0, 7, 9];
let l = splitLT t 5;
let r = splitGT t 5;

show t; show l; show r;

show $ merge l r;
