open Kit

let tests = 
"
(* 0 - 2 *)
let plus x y = x + y;
let equal x y = x = y;
(2, fun x -> plus x 1, fun x -> equal x 1);

(* 3 *)
let id = fun x -> x in
let g = fun x -> id x in
let k = fun x -> g x in
fun x -> id (g (k x)); 
(* 4 - 5 *)
let apply x y = x y;
apply (plus 2);
(* 6 - 7 *)
let flip x y z = x z y;
(fun x -> flip x, fun x -> fun y -> flip x y);

(* 8 - 10 *)
let compose x y = fun z -> x (apply y z);
(compose (plus 2), compose compose compose);
let f = flip in let c = compose in 
c c f c c f c c f c c f c c f c c f c c f c c f c c f c c f c c f c c f c c f c c f;

(* 11 - 12 *)
let f a b c d e f g = a (b c d) e (f g) in f;
let f a b c d e f g = apply a (apply b c d) e (apply f g) in f;

(* 13 *)
let rec fact x = if x = 0 then 1 else x * fact (x - 1);
"

let ans = 
[mlet ["plus", "int -> int -> int"];
 mlet ["equal", "'a -> 'a -> bool"];
 mexp "(int, int -> int, int -> bool)";
 mexp "'a -> 'a"; (* 3 *)
 mlet ["apply", "('a -> 'b) -> 'a -> 'b"]; (* 4 *)
 mexp "int -> int";
 mlet ["flip", "('a -> 'b -> 'c) -> 'b -> 'a -> 'c"]; (* 6 *)
 mexp "(('a -> 'b -> 'c) -> 'b -> 'a -> 'c, ('d -> 'e -> 'f) -> 'e -> 'd -> 'f)";
 mlet ["compose", "('a -> 'b) -> ('c -> 'a) -> 'c -> 'b"]; (* 8 *)
 mexp "(('a -> int) -> 'a -> int, \
       ('b -> 'c) -> ('d -> 'e -> 'b) -> 'd -> 'e -> 'c)";
   (* TODO: OCaml does not generalize this kind of expression.
    * But what is the harm of generalizing expressions that cannot pass on? *)
 mexp "('a -> 'b -> 'c) -> ('d -> 'a) -> 'b -> 'd -> 'c";
 (* 11 *)
 mexp "('a -> 'b -> 'c -> 'd) -> ('e -> 'f -> 'a) -> 'e -> 'f -> 'b -> ('g -> 'c) -> 'g -> 'd";
 mexp "('a -> 'b -> 'c -> 'd) -> ('e -> 'f -> 'a) -> 'e -> 'f -> 'b -> ('g -> 'c) -> 'g -> 'd";
 (* 13 *)
 mlet ["fact", "int -> int"]
]

let run () = ("BASIC", run_test tests ans)
