let run () =
  let ret = [Basic.run (); Basic1.run (); List0.run (); Variant.run ()]
  in List.iter 
    (fun (s, (status, faillst)) -> 
       begin
         Printf.printf "Case %s: %s\n" s status;
         if faillst != [] then 
           Printf.printf "\tFailed @ [%s]\n" @@
           String.concat ", " @@ List.map string_of_int faillst;
       end)
    ret
