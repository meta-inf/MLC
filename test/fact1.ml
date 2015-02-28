let rec f x y =
  if x == 0 then y
  else f (x - 1) (x * y);

dispi $ f 1000000 1;

