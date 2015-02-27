let rec f x = if x = 0 then 1 else f (x - 1) * x;

dispi (f 10);
