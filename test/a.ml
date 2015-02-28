type 'a option = None | Some of 'a;

let u = ref None;

u := Some 2;
(fun (Some x) -> dispi x) $ deref u;

u := None;


