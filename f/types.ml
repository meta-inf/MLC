
type ('a, 'b) type_tag_b = 
    TInt | TFloat | TStr | TBool | TUnit
  | TVar of 'b
  | TFunc of ('a, 'b) type_tag_b * ('a, 'b) type_tag_b
  | TTuple of ('a, 'b) type_tag_b list
  | TAlType of string * (('a, 'b) type_tag_b list)

type 'a type_tag = ('a, int) type_tag_b

type finalized
type instantiated

type 'a type_sig = (int list) * ('a type_tag) (* (free variables, tag) *)

let rec tag_finalize : instantiated type_tag -> finalized type_tag
  = function
  | TInt -> TInt
  | TFloat -> TFloat
  | TStr -> TStr
  | TBool -> TBool
  | TUnit -> TUnit
  | TVar v -> TVar v
  | TFunc (a, b) -> TFunc (tag_finalize a, tag_finalize b)
  | TTuple lst -> TTuple (List.map tag_finalize lst)
  | TAlType (a, lst) -> TAlType (a, List.map tag_finalize lst)

let rec tag_instantiate : finalized type_tag -> instantiated type_tag
  = function
  | TInt -> TInt
  | TFloat -> TFloat
  | TStr -> TStr
  | TBool -> TBool
  | TUnit -> TUnit
  | TVar v -> TVar v
  | TFunc (a, b) -> TFunc (tag_instantiate a, tag_instantiate b)
  | TTuple lst -> TTuple (List.map tag_instantiate lst)
  | TAlType (a, lst) -> TAlType (a, List.map tag_instantiate lst)

let rec iter_v f tag =
  match tag with
  | TInt | TFloat | TStr | TBool | TUnit -> ()
  | TVar v -> f v
  | TFunc (a, b) -> (iter_v f a; iter_v f b)
  | (TTuple lst | TAlType (_, lst)) -> List.iter (iter_v f) lst

let rec map_v f tag =
  match tag with
  | TInt | TFloat | TStr | TBool | TUnit -> tag
  | TVar v -> TVar (f v)
  | TFunc (a, b) -> 
    let b', a' = map_v f b, map_v f a in TFunc (a', b') (* preserve order *)
  | TTuple lst -> TTuple (List.map (map_v f) lst)
  | TAlType (a, lst) -> TAlType (a, List.map (map_v f) lst)

let isVar = function
  | TVar _ -> true
  | _ -> false

