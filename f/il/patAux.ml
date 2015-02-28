open IAst
open Ast
open Afx

type atom_value = 
    AInt of int 
  | AFloat of float 
  | ALabel of string

type access_path = int list

type trait = 
    TNil 
  | TNode of trait * access_path * atom_value

type ptrait =
    Pack of trait * int list * int list

type traitPool = Pool of (ptrait * traitPool) list

let nodeOfPack = function Pack (a, _, _) -> a

let preOfPack x = match nodeOfPack x with
  | TNode (a, _, _) -> a
  | TNil -> failwith ""

let pathOfPack x = match nodeOfPack x with
  | TNode (_, a, _) -> a
  | TNil -> failwith ""

let eqvAValue av id =
  let iv = 
    match av with
    | AInt i -> IAst.IntConst i
    | AFloat i -> IAst.FltConst i
    | ALabel s -> IAst.IntConst (Env.findOrAdd s)
  in
  IAst.FunApp (IAst.FunApp (IAst.Identifier "==", [iv]), 
               [IAst.Identifier id])


module PE =
struct
  type t = int * IntSet.t * string

  let init id =
    let ct = ref 0 in
    begin
      ct := !ct + 1;
      (!ct, IntSet.empty, id)
    end

  let rec get_path (path: access_path) ((id, e, root): t) =
    let pname = sprintf "m/%d/%s" id (String.concat "" @@ List.rev_map string_of_int path)
    and tuplesel a b = 
      IAst.(FunApp (FunApp (Identifier "tuple-sel", [IntConst a]), [b]))
    in
    if path = [] then
      ((id, e, root), Afx.id, root) 
    else if IntSet.mem (hash path) e then
      ((id, e, root), Afx.id, pname)
    else
      let (_, e', _), tf, pre = get_path (L.tl path) (id, e, root) in
      let e'' = IntSet.add (hash path) e' in
      let tf' = 
        fun x -> tf @@ 
          IAst.Let ([pname, tuplesel (L.hd path) (IAst.Identifier pre)], x)
      in
      ((id, e'', root), tf', pname)
end


let collectAlias: pattern -> (access_path * string) list =
  let rec trav path = function
    | (PInt _ | PFloat _) -> []
    | PVar s -> [path, s]
    | PTuple l -> 
      let lst = L.mapi (fun i e -> trav ((i + 1) :: path) e) l in
      L.concat lst
    | PAlType (s, l) -> 
      let lst = L.mapi (fun i e -> trav ((i + 1) :: path) e) l in
      L.concat lst
    | PAlias (s, v) -> (path, s) :: (trav path v)
    (* When called in expandPOr, we are not interested in the path *)
    | POr (x, y) -> trav path x 
  in fun x -> trav [] x

module IE =
struct
  type t = { mutable mayfail : bool;
             mutable suc     : IntSet.t;
             id              : int;
             matchee         : string;
             param           : access_path list list;
             impl            : iexpr list }

  let action_id e = sprintf "m/%da/%d" e.id

  let init: string -> (Ast.pattern * IAst.iexpr) list -> t =
    let ct = ref 0 in
    fun id code ->
      let param0, impl0 =
        L.split @@ L.map 
          (fun (pat, action) -> 
             let path, als = L.split @@ collectAlias pat in
             let als = if als = [] then ["m/d"] else als in
             (path, IAst.Lambda (als, action)))
          code
      in
      begin
        ct := !ct + 1;
        { mayfail = false;
          suc = IntSet.empty;
          id = !ct;
          matchee = id;
          param = param0;
          impl = impl0 }
      end

  let finalize e =
    let bindings = 
      IntSet.elements e.suc 
      |> L.map (fun i -> (action_id e i, L.nth e.impl i))
    in 
    fun e -> IAst.Let (bindings, e)

  let match_failure e = 
    begin
      e.mayfail <- true;
      IAst.(FunApp (Identifier "match_failure", [IntConst e.id]))
    end

  let mscall (caller, callee) =
    let caller = Identifier caller in
    let callee = 
      if callee = [] then [IAst.IntConst 0] 
      else callee |> L.map (fun x -> IAst.Identifier x)
    in
    List.fold_left (fun res cur -> IAst.FunApp (res, [cur])) caller callee

  let match_success i pe ie = 
    let names, _, tf =
      L.fold_left 
        (fun (names, pe, tf) path -> 
           let (pe', tf', name) = PE.get_path path pe in
           (names @ [name], pe', fun e -> tf (tf' e)))
        ([], pe, fun x -> x)
        (List.nth ie.param i)
    in
    begin
      ie.suc <- IntSet.add i ie.suc;
      tf (mscall (action_id ie i, names))
    end
end


