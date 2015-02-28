type 'a option = None | Some of 'a;

let u = ref None;

u := Some 2;
(fun (Some x) -> dispi x) $ deref u;

disp "\n";

(* u := Some "2"; <- error *)

let show =
  let ivk =
    let cont = ref 0 in
    fun a ->
      begin
        cont := (deref cont) + 1;
        dispi $ deref cont;
        disp "\n"
      end
  in
  let rec iter n = 
    match n with
    | 0 -> 0
    | n -> begin ivk 0; iter $ n - 1 end
  in iter 10
;
