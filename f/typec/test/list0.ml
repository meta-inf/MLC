open Kit

let tests = 
"
[[], []];

fun x -> fun y -> [x] :: [[x + 1]];

let con a b = a :: b;

let revmap f la = 
  let rec mapIter la lb = 
    match la with
    | [] -> lb
    | x :: xs -> mapIter xs ((f x) :: lb)
  in mapIter la [];

revmap (con 1) [[1], [2], []];

let rec merge cmp l1 l2 =
  match (l1, l2) with
  | ([], l2) -> l2
  | (l1, []) -> l1
  | (h1 :: t1, h2 :: t2) ->
      if cmp h1 h2 < 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;

let rec chop k l =
  if k = 0 then l else
    match l with
    | x :: t -> chop (k - 1) t
;

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

let partition p l =
  let rec part yes no = function x -> match x with
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l 
  in part [] [] l
;

let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1 :: l1, a2 :: l2) -> (a1, a2) :: combine l1 l2
;

let flip f x y = f y x;

let concat a b = rev_append (rev a) b;

let qsort lt lis =
  let rec traverse l =
    match l with
    | [] -> []
    | x :: xs -> 
      concat 
        (traverse (filter (fun y -> lt y x) xs)) 
        (x :: (traverse (filter (fun y -> lt x y) xs)))
  in traverse lis;
"

let ans = 
  [mexp "('a list) list";
   mexp "int -> 'a -> (int list) list";
   mlet ["con", "'a -> 'a list -> 'a list"];
   mlet ["revmap", "('a -> 'b) -> 'a list -> 'b list"];
   mexp "(int list) list";
   mlet ["merge", "('a -> 'a -> int) -> 'a list -> 'a list -> 'a list"];
   mlet ["chop", "int -> 'a list -> 'a list"];
   mlet ["rev_append", "'a list -> 'a list -> 'a list"];
   mlet ["rev", "'a list -> 'a list"];
   mlet ["find_all", "('a -> bool) -> 'a list -> 'a list"];
   mlet ["filter", "('a -> bool) -> 'a list -> 'a list"];
   mlet ["partition", "('a -> bool) -> 'a list -> ('a list, 'a list)"];
   mlet ["combine", "'a list -> 'b list -> ('a, 'b) list"];
   mlet ["flip", "('a -> 'b -> 'c) -> 'b -> 'a -> 'c"];
   mlet ["concat", "'a list -> 'a list -> 'a list"];
   mlet ["qsort", "('a -> 'a -> bool) -> 'a list -> 'a list"]]
  
let run () = ("LIST", run_test tests ans)
