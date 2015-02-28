open Afx
open Types

let rename (lst, tag) =
  let st = ref IntMap.empty in
  map_v (
    fun i -> 
      if List.exists ((=) i) lst then
        try IntMap.find i !st 
        with Not_found -> 
          let u = IntMap.cardinal !st in
          (st := IntMap.add i u !st; u)
      else -1 - i)
    tag

let rec string_of_tag t = 
  let closed = function
    | TFunc _ | TAlType _ -> false
    | _ -> true
  in
  let enclose f = 
    if closed f then string_of_tag f 
    else Printf.sprintf "(%s)" @@ string_of_tag f 
  in
  let enclose1 f =
    match f with
    | TFunc _ -> Printf.sprintf "(%s)" @@ string_of_tag f 
    | _ -> string_of_tag f
  in
  match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TStr -> "string"
  | TUnit -> "unit"
  | TBool -> "bool"
  | TVar v -> 
    if v >= 0 then Printf.sprintf "'%c" (char_of_int (int_of_char 'a' + v))
    else Printf.sprintf "'_%c" (char_of_int (int_of_char 'a' - (v + 1)))
  | TFunc (f, v) -> 
    Printf.sprintf "%s -> %s" (enclose1 f) (string_of_tag v)
  | TTuple lst -> 
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_tag lst))
  | TAlType (a, lst) ->
    match lst with
    | [] -> a
    | [p] -> Printf.sprintf "%s %s" (enclose p) a
    | _ -> 
      let par_str = String.concat ", " (List.map string_of_tag lst) in
      Printf.sprintf "(%s) %s" par_str a

let printTag v = string_of_tag (rename v)
