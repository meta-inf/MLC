(* value type for interpreter *)

exception Runtime_exn of string

module IntMap = Map.Make(struct type t = int let compare = compare end)

type value =
    IntVal of int
  | FltVal of float
  | BoolVal of bool
  | StrVal of string
  | BuiltinFunc of (value -> value) (* pure functions *)
  | BuiltinFuncS of (value -> refTable -> value) (* with side effect *)
  | Func of int * Ast.expr * valTable
  | RefVal of int
  | Tuple of (value array)
  | Unit
and vtflag = RecV | NormalV
and valTable = (value * vtflag) IntMap.t
and refTable = (value IntMap.t) ref

let v2i = function
  | IntVal v -> v
  | _ -> failwith "v2i"
and v2f = function
  | FltVal v -> v
  | _ -> failwith "v2f"
and v2b = function
  | BoolVal v -> v
  | _ -> failwith "v2b"

module RefTable = 
struct
  let empty = IntMap.empty
  let find id tbl = IntMap.find id (!tbl)
  let modify id value tbl = tbl := IntMap.add id value (!tbl)
  let add value tbl = 
    let ret = IntMap.cardinal (!tbl)
    in (tbl := IntMap.add ret value (!tbl); ret)
end

type builtinBinFuncType = 
    PureBF of (value -> value -> value)
  | ImpureBF of (value -> value -> refTable -> value)

let identical x y =
  match x with
  | IntVal _ | FltVal _ | BoolVal _ | Unit | RefVal _ -> x = y
  | BuiltinFunc _ | BuiltinFuncS _ | Func _ -> false
  | StrVal s -> (match y with StrVal t -> s == t | _ -> false)
  | Tuple l -> (match y with Tuple r -> l == r | _ -> false)

let builtinBinOps = 
  [("=", PureBF (fun x y -> BoolVal (x = y)));
   ("==", PureBF (fun x y -> BoolVal (identical x y)));
   ("<", PureBF (fun x y -> BoolVal (x < y)));
   (">", PureBF (fun x y -> BoolVal (y < x)));
   ("+", PureBF (fun x y -> IntVal ((v2i x) + (v2i y))));
   ("-", PureBF (fun x y -> IntVal ((v2i x) - (v2i y))));
   ("*", PureBF (fun x y -> IntVal ((v2i x) * (v2i y))));
   ("/", PureBF (fun x y -> IntVal ((v2i x) / (v2i y))));
   ("or", PureBF (fun x y -> BoolVal ((v2b x) || (v2b y))));
   ("tuple-sel", PureBF (fun x y -> match (y, x) with 
        | (Tuple arr, IntVal k) -> arr.(k)
        | _ -> failwith "tuple-sel: invalid type"));
   ("set-ref", ImpureBF (fun x y vtable -> match x with
        | RefVal id -> (RefTable.modify id y vtable; Unit)
        | _ -> failwith "set-ref: not a reference"))
  ]

let builtinFuncList =
  let binOp = function
    | PureBF f -> BuiltinFunc (fun x -> BuiltinFunc (fun y -> (f x y)))
    (* Since vt (as a ref) will not change during the session *)
    | ImpureBF f -> BuiltinFunc (fun x -> BuiltinFuncS (fun y vt -> (f x y vt)))
  in
  [("not", BuiltinFunc (fun x -> BoolVal (not (v2b x))));
   ("id", BuiltinFunc (fun x -> x));
   ("match_failure", BuiltinFunc (fun _ -> raise @@ Runtime_exn "Match_failure"));
   ("disp", BuiltinFunc (fun x -> match x with
        | StrVal s -> (Printf.printf "%s" s; Unit)
        | _ -> failwith "disp: not a string"));
   ("dispi", BuiltinFunc (fun x -> match x with
        | IntVal s -> (Printf.printf "%d" s; Unit)
        | _ -> failwith "dispi: not a integer"));
   ("ref", BuiltinFuncS (fun va vtable -> RefVal (RefTable.add va vtable)));
   ("deref", BuiltinFuncS
      (fun va vtable -> match va with
         | RefVal id -> RefTable.find id vtable
         | _ -> failwith "deref: not a reference"));
   ("$", Func (0, Ast.(Lambda ([1], FunApp (Identifier 0, Identifier 1))), 
               IntMap.empty)) (* reduceBinOp checked this problem *)
  ] @
  (List.map (function (s, f) -> (s, binOp f)) builtinBinOps)


module Table =
struct
  let find var env =
    match (IntMap.find var env) with
    | (va, NormalV) -> va
    | (Func(id, body, _), RecV) -> Func(id, body, env)
    | (other, RecV) -> other
  (* `letrec f = let p = 2 in function -> ...` is not supported *)
  let add id value env = 
    IntMap.add id (value, NormalV) env 

  let add_r id value env = IntMap.add id (value, RecV) env

  let rec loadBuiltinFunc lst table =
    match lst with
    | [] -> table
    | (str, f) :: lst' -> loadBuiltinFunc lst' (add (Parse.IdTable.find str) f table)

  let empty = loadBuiltinFunc builtinFuncList IntMap.empty
end
