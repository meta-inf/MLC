open Kit

let tests = 
"(* 0 - 2 *)
type 'a tree = 
  Leaf of 'a 
| Branch of ('a, 'a tree, 'a tree);

(Leaf 0, Leaf (Leaf 0),
 let a = Leaf 0 in let b = Leaf 1 in Branch (3, a, b));

type ('a, 'b) treap =
  Leaf
| Node of ('b, 'a, ('a, 'b) treap, ('a, 'b) treap);

Leaf;

let rec fmap f t =
  match t with
  | Leaf -> Leaf
  | Node (prio, key, lch, rch) -> Node (prio, f key, fmap f lch, fmap f rch);
"

let ans = 
  [mnone;
   mexp "(int tree, (int tree) tree, int tree)";
   mnone;
   mexp "('a, 'b) treap";
   mlet ["fmap", "('a -> 'b) -> ('a, 'c) treap -> ('b, 'c) treap"];
  ]

let run () = ("VARIANT", run_test tests ans) 
