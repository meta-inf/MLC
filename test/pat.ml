let rec map f a =
  match a with
  | [] -> []
  | x :: rs -> (f x) :: (map f rs)
;

let showlst l = 
  begin
    map (fun x -> begin dispi x; disp " " end) l;
    disp "\n"
  end;

let (x, y) = (2, 3) in showlst [x, y];
let (x, (y, z)) = (2, (3, 4)) in showlst [x, y, z];
let (2, (y, 4)) = (2, (3, 4)) in showlst [y];

let a :: rst = [2, 3, 4] in showlst rst;
let a :: rst = [[2], [3, 4], [4, 5]] in map showlst rst;
