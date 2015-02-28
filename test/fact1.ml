let rec f x y =
  if x == 0 then y
  else f (x - 1) (x * y);

dispi $ f 100000 1;

