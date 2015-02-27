let rec f x y =
  if x == 0 then y
  else f (x - 1) (x * y)
in dispi (f 100000 1);

