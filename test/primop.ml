let b2i x = if x then 1 else 0;

dispi (b2i (2 == 3));

let f x = begin dispi (b2i (2 == x)); dispi (b2i (x == 2)) end
in begin f 3; f 2 end;

