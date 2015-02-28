let rec map f a =
  match a with
  | [] -> []
  | x :: rs -> (f x) :: (map f rs)
;

let showlst l = 
  begin
    map (fun x -> begin dispi x; disp " " end) l;
    disp "\n"
  end;

showlst [1, 2];

let rec merge cmp l1 l2 =
  match (l1, l2) with
  | ([], l2) -> l2
  | (l1, []) -> l1
  | (h1 :: t1, h2 :: t2) ->
      if cmp h1 h2 < 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;

showlst (merge (fun x y -> x - y) [1, 3] [2]);


let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2);

let rev l = rev_append l [];

let find_all p =
  let rec find accu = function x -> match x with
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find [];

let filter = find_all;

let concat a b = rev_append (rev a) b;

let qsort lis =
  let rec traverse l =
    match l with
    | [] -> []
    | x :: xs -> 
      concat 
        (traverse (filter (fun y -> y < x) xs)) 
        (x :: (traverse (filter (fun y -> x < y) xs)))
  in traverse lis;

let a = qsort [1, 3, 2, 5, 4] in showlst a;
