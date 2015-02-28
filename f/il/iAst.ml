type iexpr =
    IntConst of int
  | FltConst of float
  | BoolConst of bool
  | StrConst of string
  | Identifier of string
  | Cond of (iexpr * iexpr) list
  | Lambda of (string list) * iexpr
  | FunApp of iexpr * (iexpr list) (* general functions *)
  | Let of (string * iexpr) list * iexpr
  | LetRec of (string * iexpr) list * iexpr
  | Seq of iexpr list
  | Tuple of iexpr list
;;

let makeIf (x, y, z) = 
  Cond [x, y; BoolConst true, z]


let rec map_e f = function
  | (IntConst _ | FltConst _ | BoolConst _ | StrConst _) as v -> v
  | Identifier v -> Identifier (f v)
  | Cond lst -> 
    Cond (List.map (fun (x, y) -> (map_e f x, map_e f y)) lst)
  | Lambda (sl, v) -> 
    Lambda (sl, 
            map_e (fun s -> if List.mem s sl then s else f s) v)
  | FunApp (u, v) -> FunApp (map_e f u, List.map (map_e f) v)
  | Let (lst, e) -> 
    Let (
      List.map 
        (fun (sl, e) -> 
           (sl, map_e (fun s -> if s = sl then s else f s) e))
        lst,
      map_e f e)
  | LetRec (lst, e) -> 
    LetRec (
      List.map 
        (fun (sl, e) -> 
           (sl, map_e (fun s -> if s = sl then s else f s) e))
        lst,
      map_e f e)
  | Tuple l -> Tuple (List.map (map_e f) l)
  | Seq l -> Seq (List.map (map_e f) l)


let printf = Printf.printf

let rec atomic = function
  | Cond _ | Lambda _ | FunApp _ | Let _ | LetRec _ -> false
  | _ -> true

and vprint t = 
  match t with
  | IntConst v -> printf "%d" v
  | FltConst v -> printf "%f" v
  | StrConst v -> printf "\"%s\"" v
  | BoolConst v -> printf (if (v) then "#t" else "#f")
  | Identifier v -> printf "%s" v

  | Cond lst -> 
    begin
      printf "(cond";
      let pfunc i (cond, exp) =
        begin
          printf "\n(";
          vprint cond;
          printf "\n";
          vprint exp;
          printf ")";
        end
      in List.iteri pfunc lst;
      printf ")";
    end
  | FunApp (func, param) ->  (* TODO *)
    (printf "("; vprint func; 
     List.iter (fun p -> (printf " "; vprint p)) param; printf ")")

  | Lambda (ids, value) -> begin
      printf "(fun (%s)\n" (String.concat " " ids);
      vprint value;
      printf ")";
    end
  | Let (lst, value) -> begin
      printf "(let \n(";
      let pfunc i (id, value) =
        begin
          if i > 0 then printf "\n";
          printf "(%s " id;
          vprint value;
          printf ")";
        end
      in List.iteri pfunc lst;
      printf ")\n";
      vprint value;
      printf ")";
    end
  | LetRec (lst, value) -> begin
      printf "(letrec \n(";
      let pfunc i (id, value) =
        begin
          if i > 0 then printf "\n";
          printf "(%s " id;
          vprint value;
          printf ")";
        end
      in List.iteri pfunc lst;
      printf ")\n";
      vprint value;
      printf ")";
    end
  | Tuple lst -> begin
      printf "(tuple";
      let pfunc i v =
        begin
          printf " ";
          vprint v;
        end
      in List.iteri pfunc lst;
      printf ")";
    end
  | Seq lst -> 
    let tl, rst = 
      let l1 = List.rev lst in
      (List.hd l1, List.rev (List.tl l1))
    in
    begin
      printf "(let \n(";
      let pfunc i v =
        begin
          if i > 0 then printf "\n";
          printf "(_ ";
          vprint v;
          printf ")";
        end
      in List.iteri pfunc rst;
      printf ")\n";
      vprint tl;
      printf ")";
    end

let printAst v = begin vprint v; printf "\n"; end


