let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1 :: l1, a2 :: l2) -> (a1, a2) :: combine l1 l2
;

let split = 
  let rec trav lst (r1, r2) = 
    match lst with
    | [] -> (r1, r2)
    | (a, b) :: rst -> trav rst (a :: r1, b :: r2)
  in fun x -> trav x ([], [])
;

let rec map f a =
  match a with
  | [] -> []
  | x :: rs -> (f x) :: (map f rs)
;

let rec length x =
  match x with
  | [] -> 0
  | a :: rs -> 1 + (length rs)
;

let showlst l = map (fun x -> dispi x) l;
let fst (a, b) = a;
let snd (a, b) = b;

dispi (length [2]);

showlst (fst (split (combine [2, 4] [1, 3])));
