let add a b = a + b;
(add 2 3, add 2, let u = add 2 in u 3);
let sel31 a b c = a;

sel31 3 2; (* un-uncurried *)
dispi $ sel31 2 3 4; (* uncurried *)
let u = sel31 2 in u 3 4; (* un-uncurried (for now) *)

dispi $ sel31 (fun x -> x) 2 3 4; (* do not over-uncurry *)

let or a = a in or (fun x -> x) 2; (* un-uc; primitive shadow test *)
let f or a b = or a b; (* un-uc; primitive shadow test *)
dispi $ (fun or a b -> or a b) (fun a b -> a) 2 3; (* un-uc; primitive is shadowed *)


