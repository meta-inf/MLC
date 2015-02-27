let rec map f a =
  match a with
  | [] -> []
  | x :: rs -> (f x) :: (map f rs)
;

map (fun x -> x) [2];
