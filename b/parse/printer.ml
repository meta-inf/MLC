open Printf
open Ast

let rec printList pfunc lst = ignore 
    (List.mapi (fun id parg -> begin
           if (id > 0) then printf " " else ();
           pfunc parg;
         end) lst)

let rec printAstT t = match t with
  | IntConst v -> printf "%d" v
  | FltConst v -> printf "%f" v
  | StrConst v -> printf "\"%s\"" v
  | BoolConst v -> printf (if (v) then "#t" else "#f")
  | Identifier v -> printf "v%d" v
  | Cond lst -> begin
      printf "(cond ";
      let pfunc (cond, value) =
        begin
          printf "(";
          printAstT cond;
          printf " ";
          printAstT value;
          printf ")";
        end
      in printList pfunc lst;
      printf ")";
    end
  | FunApp (func, param) -> begin
      printf "(";
      printAstT func;
      List.iter (fun p -> (printf " "; printAstT p)) param;
      printf ")";
    end
  | FunApp1 (id, param) -> begin
      printf "(v%d " id;
      printf " ";
      printAstT param;
      printf ")";
    end
  | FunApp2 (id, el, er) -> begin
      printf "(v%d " id;
      printAstT el;
      printf " ";
      printAstT er;
      printf ")";
    end
  | Lambda (ids, value) -> begin
      printf "(fun (%s) " (String.concat " " @@ List.map string_of_int ids);
      printAstT value;
      printf ")";
    end
  | Let (id, value, k) -> begin
      printf "(let ";
      let pfunc (id, value) =
        begin
          printf "(v%d " id;
          printAstT value;
          printf ")";
        end
      in pfunc (id, value);
      printf " ";
      printAstT k;
      printf ")";
    end
  | LetRec (lst, value) -> begin
      printf "(letrec (";
      let pfunc (id, value) =
        begin
          printf "(v%d " id;
          printAstT value;
          printf ")";
        end
      in printList pfunc lst;
      printf ") ";
      printAstT value;
      printf ")";
    end
  | Tuple lst -> begin
      printf "(tuple ";
      printList printAstT lst;
      printf ")";
    end

let printAst v = begin
  printAstT v;
  printf "\n";
end
(*
printAst (Cond [(BoolConst true, BoolConst false); (BoolConst false, BoolConst true)]);;
printAst (LetRec([(0, BoolConst false); (1, IntConst 3); (2, FltConst 3.0)], IntConst 0));;
*)
