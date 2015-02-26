open Cps
open Mvalue
open Printf

let rec soi0 n =
  if (n < 26) then String.make 1 (char_of_int (n + (int_of_char 'a')))
  else if (n < 52) then String.make 1 (char_of_int (n + (int_of_char 'A') - 26))
  else String.concat "" [soi0 (n / 52); soi0 (n mod 52)]

let soi =
  let lst = List.map (fun (x, y) -> (y, x)) @@ Parse.IdTable.(IdMap.bindings tbl_orig) in
  let lst_max = List.fold_left (fun x (y, z) -> (max x y)) 0 lst
  in fun x ->
    try List.assoc x lst
    with Not_found -> 
      begin
        assert(x > lst_max);
        soi0 (x - lst_max - 1)
      end

let pv = function
  | Var (ID i) -> printf "%s" (soi i)
  | Int i -> printf "%d" i
  | Float i -> printf "%.2f" i
  | Str s -> printf "\"%s\"" s
  | Bool i -> printf "%s" (if i then "#t" else "#f")
  | Unit -> printf "()"
  | ClosureVal (ID i) -> printf "c#%d" i
and pi (ID i) = printf "%s" (soi i)
and ps () = printf " "
and p s = printf "%s" s
and ign = ignore

let print =
  let rec print0 idt x = 
    (ign Array.(map p (make idt " "));
     match x with
     | CTerm -> (p "term\n")
     | CValue (v, k) -> (pv v; print idt k)
     | CPrimOp1 (i, v, k) -> (p (soi i); ps (); pv v; print idt k)
     | CPrimOp2 (i, l, r, k) ->
       (p (soi i); ps (); pv l; ps (); pv r; print idt k)
     | CCond (i, k1, k2) ->
       (p "cond "; pv i; p "\n"; print0 (idt + 1) k1; p "\n"; print0 (idt + 1) k2)
     | CFunApp (v, lst) ->
       (p "call "; pv v; List.iter (fun x -> (ps (); pv x)) lst)
     | CTuple (lst, k) ->
       (p "tuple"; List.iter (fun x -> (ps (); pv x)) lst; print idt k)
     | CFix (lst, arr, ki) ->
       (p "fix closure =";
        Array.iter (fun i -> (p " "; pv i)) arr;
        p "\n";
        List.iter (fun (f, xl, i) -> 
            begin
              ign Array.(map p (make (idt + 1) " "));
              pi f; 
              List.iter (fun x -> (ps (); pi x)) xl;
              p " =\n";
              print0 (idt + 2) i;
              p "\n"
            end) lst;
        print0 (idt + 1) ki))
  and print idt (CExp (i, x)) =
    (p " =: "; pi i; p "\n"; print0 idt x)
  in fun x -> print0 0 x

let rec mv2s = function
  | StkVar i -> sprintf "s%d" i
  | CloVar i -> sprintf "c%d" i
  | Reg i -> sprintf "r%d" i
  | RegF i -> sprintf "xmm%d" i
  | CloRef (i, None, j) -> sprintf "cn%s.%s" (mv2s i) (soi j)
  | CloRef (i, Some v, j) -> sprintf "c%s.%s(%s)" (mv2s i) (soi j) (mv2s v)
  | Imm i -> sprintf "imm%d" i
  | ImmF i -> sprintf "imm%.2f" i
  | ImmS i -> sprintf "str[%d]" i
  | Label i -> sprintf "fn%d" i

let rec printR x = 
  match x with
  | MPrimOp1 (_, id, d, s, k) ->
    begin
      printf "%s = %s %s\n" (mv2s d) (soi id) (mv2s s);
      printR k;
    end
  | MPrimOp2 (_, id, d, s0, s1, k) ->
    begin
      printf "%s = %s %s %s\n" (mv2s d) (mv2s s0) (soi id) (mv2s s1);
      printR k;
    end
  | MCond (_, v, k1, k2) ->
    begin
      printf "cond %s\n" (mv2s v);
      printR k1;
      printf "cond %s - false\n" (mv2s v);
      printR k2;
    end
  | MFunApp (_, v, lst) ->
    begin
      printf "call %s" (mv2s v);
      List.iter (fun x -> printf " %s" (mv2s x)) lst;
      printf "\n";
    end
  | MTuple (_, mv, lst, k) ->
    begin
      printf "%s = tuple" (mv2s mv);
      List.iter (fun x -> printf " %s" (mv2s x)) lst;
      printf "\n";
      printR k;
    end
  | MCopy (_, dst, src, k) ->
    begin
      printf "mov %s %s\n" (mv2s dst) (mv2s src);
      printR k;
    end
  | MTerm -> printf "<term>\n"
