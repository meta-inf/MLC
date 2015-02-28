let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1 :: l1, a2 :: l2) -> (a1, a2) :: combine l1 l2
;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2);

let rev l = rev_append l [];

let fst (a, b) = a;
let snd (a, b) = b;

let split = 
  let rec trav lst (r1, r2) = 
    match lst with
    | [] -> (r1, r2)
    | (a, b) :: rst -> trav rst (a :: r1, b :: r2)
  in
  fun x -> 
    let r = trav x ([], []) in 
    (rev $ fst r, rev $ snd r)
;

let rec map f a =
  match a with
  | [] -> []
  | x :: rs -> (f x) :: (map f rs)
;

let rec length = function
  | [] -> 0
  | a :: rs -> 1 + (length rs)
;

let showlst l = 
  begin
    map (fun x -> begin dispi x; disp " " end) l;
    disp "\n"
  end;

showlst $ map ((+) 2) [2, 3, 4];

showlst $ fst $ split $ combine [2, 4] [1, 3];
showlst $ snd $ split $ combine [2, 4] [1, 3];
