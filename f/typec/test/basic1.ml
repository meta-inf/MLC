open Kit

let tests = 
"(* 0 - 2 *)
[];
([2], [[2], [3, 4], []], [[], [[[]]]]);

let concat x y =
  let rec trav l =
    match l with
    | [] -> y
    | x :: xs -> x :: trav xs
  in trav x;

let rec sum y w x f zero = 
  match x with
  | [] -> (y, w, f zero)
  | x :: xs -> 
	let tadd a b = 
      match (a, b) with
        ((x, y, z), (w, r, s)) -> (x + w, y + r, z + s)
	in tadd (y, w, f x) (sum y w xs f zero)
;

let rec accu plus sum f m1 zero icur =
	if icur == zero then sum else
	accu plus (plus (f icur) sum) f m1 zero (m1 icur);
"

let ans = 
  [mexp "'a list";
   mexp "(int list, (int list) list, ((('a list) list) list) list)";
   mlet ["concat", "'a list -> 'a list -> 'a list"];
   mlet ["sum", "int -> int -> 'a list -> ('a -> int) -> 'a -> (int, int, int)"];
   mlet ["accu", "('a -> 'b -> 'b) -> 'b -> ('c -> 'a) -> ('c -> 'c) -> 'c -> 'c -> 'b"]
  ]

let run () = ("BASIC1", run_test tests ans) 
