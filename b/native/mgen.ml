(* mgen.ml: machine code generation *)

open Afx
open Mvalue
open Register

let concat = String.concat ""
let print = Printf.sprintf

let v2s = function
  | StkVar i -> print "[rbp - %d]" (8 * (i + 2))
  | CloVar i -> print "[r15 + %d]" (8 * (i + 1))
  | Reg i    -> 
    if i = 15 then "r15" else
      [|"rax"; "rbx"; "rcx"; "rdx"; "rsi"; "rdi";
        "r8"; "r9"; "r10"; "r11"; "r12"; "r13"|].(i)
  | Imm i    -> string_of_int (i * 2)
  | ImmS i   -> print "str%d + 8" i
  | Label i  -> print "fn%d" i
  | CloRef _ -> failwith "cloref"
  | _ -> failwith "ni"

let l8bit = 
  let tbl = ["rax", "al"; "rbx", "bl"; "rcx", "cl"; "rdx", "dl";
             "rsp", "spl"; "rbp", "bpl"; "rsi", "sil"; "rdi", "dil";
             "r8", "r8b"; "r9", "r9b"; "r10", "r10b"; "r11", "r11b";
             "r12", "r12b"; "r13", "r13b"; "r14", "r14b"; "r15", "r15b"]
  in fun x -> try List.assoc (v2s x) tbl
    with Not_found -> raise @@ Invalid_argument "l8bit: not a register?"

module AD =
struct
  type t = 
    | R of string
    | M of string
    | I of string (* imm, immF *)
    | L of string (* immS, label *)
  and sigt = Rt | Mt | It | Lt

  let typeof = function
    | R _ -> Rt
    | M _ -> Mt
    | I _ -> It
    | L _ -> Lt

  let fromMV s =
    match s with
    | Reg _ | RegF _ -> R (v2s s)
    | StkVar _ | CloVar _ -> M (v2s s)
    | Imm _ -> I (v2s s)
    | ImmS _ | Label _ -> L (v2s s)
    | CloRef _ -> failwith "cloref"
    | _ -> failwith "ni"

  let toS = function
    | R s -> s | I s -> s | M s -> s | L s -> s

  let toAS = function
    | R s -> s
    | I s -> s
    | M s -> String.concat "" ["QWORD "; s]
    | L s -> s

  let op2 s1 a b = print "%s\t%s, %s" s1 a b
  let op1 s1 a = print "%s\t%s" s1 a
  let optional pred s1 = if pred then s1 else ""
  let optionalL pred s1 = if pred then s1 else []

  let _mov p1 p2 = 
    if p1 = p2 then ""
    else if toS p1 = "r15" then op2 "movshr" "r15" (toAS p2) (* p2 will not be L *)
    else if toS p2 = "r15" then op1 "savetup" (toAS p1)
    else match (p1, p2) with
      | (R s1, L s2) -> op2 "movshl" s1 s2
      | (R s1, _) ->    op2 "mov" s1 (toS p2)
      | (M s1, R s2) -> op2 "mov" s1 s2
      | (M s1, I s2) -> op2 "mov" (print "QWORD %s" s1) s2
      | (M s1, L s2) -> op2 "movshl" (print "QWORD %s" s1) s2
      | (M s1, M s2) -> op2 "movmem" s1 s2
      | _ -> failwith "AD._mov: invalid combination of operands"

  let mov mv1 mv2 = _mov (fromMV mv1) (fromMV mv2)

  let _push p1  = op1 "push" (toAS p1)
  let _pop p1   = op1 "pop" (toAS p1)
  let jmp s     = op1 "jmp" s
  let idiv mv   = op1 "idiv" (toAS (fromMV mv))
  let pop mv1   = _pop (fromMV mv1)
  let push mv1  = _push (fromMV mv1)
end

let moveList (src: AD.t list) (dst: AD.t list) = (* suppose [rsp-m] is not used *)
  let rec loadD lst = 
    match lst with
    | [] -> []
    | (a, b) :: [] -> (assert(a = b); [])
    | (tl, hd) :: r ->
      (AD._push hd) :: (loadC r) @ [AD._pop tl]
  and loadC lst =
    try 
      let (tl, hd) = List.find (fun (_, s) -> not (List.mem_assoc s lst)) lst
      in
      (AD._mov hd tl) :: (loadC (List.remove_assoc tl lst))
    with Not_found -> loadD lst
  in loadC (List.combine src dst)

let genExternCall fn args rm dst isva_arg = 
  (* suppose all arguments and result are passed as integer *)
  let genArgList len =
    let rec gen0 c =
      if c = 0 then []
      else (AD.M (print "[rsp - 8 * %d]" c)) :: (gen0 (c - 1))
    in
    let aregmap = [|"rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"|] in
    if len <= 6 then
      List.map (fun x -> AD.R x) @@ Array.to_list (Array.sub aregmap 0 len)
    else
      List.map (fun x -> AD.R x) (Array.to_list aregmap) @ (gen0 (len - 6))
  in
  
  let regs = (* caller-saved registers excluding dst *)
    List.filter (fun s -> not (s = dst))
      @@ List.map (fun x -> Reg x) (RM.aliveRegList rm)
  and args =
    List.map AD.fromMV args

  in List.concat
    [List.map AD.push regs; (* backup registers *)
     moveList args (genArgList (List.length args));
     if (List.length args) > 6 then
       [AD.op2 "sub" "rsp" (string_of_int @@ 8 * (List.length args - 6))]
     else
       [];
     if isva_arg then ["xor\trax, rax"] else [];
     [AD.op1 "call" fn];
     if (List.length args) > 6 then (* restore stack frame *)
       [AD.op2 "add" "rsp" (string_of_int @@ 8 * (List.length args - 6))]
     else
       [];
     [AD.mov dst (Reg 0)];
     List.rev_map AD.pop regs] (* restore regs *)


let rec genPrimOp1 id dst p1 rmap = 
  match snd (Parse.IdTable.str_of_id id) with
  | "not" ->    [AD.mov dst p1; AD.op1 "not" (v2s dst)]
  | "/" ->      [AD.mov dst p1; AD.idiv dst] (* check buildIDIV *)
  | "disp" ->   genExternCall "disp" [p1] rmap dst false
  | "dispi" ->  genExternCall "dispi" [p1] rmap dst false
  | "deref" ->  [AD.op2 "movshr" "r14" (v2s p1);
                 AD.(_mov (fromMV dst) (M "[r14 + 8]"))]
  | "ref" ->    genExternCall "make_tuple" 
                  [Imm 2; Imm (0x1ef lsl 40); p1] rmap dst true
  | "match_failure" ->
                genExternCall "match_failure" [p1] rmap dst false
  | _ -> failwith "genPrimOp1: not implemented";

and genPrimOp2 id dst p1 p2 rmap = 
  let opname = snd (Parse.IdTable.str_of_id id) in
  match opname with
  | "+" -> [AD.mov dst p1; AD.op2 "add" (v2s dst) (v2s p2)]
  | "-" -> [AD.mov dst p1; AD.op2 "sub" (v2s dst) (v2s p2)]
  | "*" -> [AD.mov dst p1;
            AD.op2 "imul"  (v2s dst) (v2s p2);
            AD.op2 "shr"   (v2s dst) "1"]
  |"or" -> [AD.mov dst p1; AD.op2 "or"  (v2s dst) (v2s p2)];
  |"==" -> (
    let p1, p2 = 
      match p1 with Imm _ | ImmS _ | Label _ -> p2, p1 | _ -> p1, p2 in
    match p1 with
    | Imm _ | ImmS _ | Label _ -> failwith "Should have been folded"
    | _ -> [AD.op2 "cmp"   (v2s p1) (v2s p2);
            AD.op1 "setz"  (l8bit dst);
            AD.op2 "movzx" (v2s dst) (l8bit dst)];)
  | "=" -> genExternCall "equiv" [p1; p2] rmap dst false
  | "<" -> genExternCall "polyLT" [p1; p2] rmap dst false
  | ">" -> genExternCall "polyLT" [p2; p1] rmap dst false
  | "tuple-sel" -> ( 
    match p1 with
    | Imm d -> [AD.op2 "movshr" "r14" (v2s p2);
                AD.(_mov (fromMV dst) (M (print "[r14 + %d]" (8 * d))))]
               @
               (if (d = 0) then
                  [AD.(op2 "mov" "r14" "0xffffffffffff");
                   AD.(op2 "and" (toAS @@ fromMV dst) "r14") ]
                else [])
    | _ -> raise @@ Invalid_argument "tuple-sel: invalid operand")
  | ":=" -> [AD.op2 "movshr" "r14" (v2s p1);
             AD.(_mov (M "[r14 + 8]") (fromMV p2))]
  | _ -> raise Not_found;

and genCond =
  let cnt = ref 0 in
  fun mv k1 k2 -> 
    (cnt := !cnt + 1;
     let i1 = print "cf%d" !cnt in
     List.concat [[AD.op2 "cmp" (v2s mv) "0";
                   AD.op1 "jz" i1];
                  gen0 k1;
                  [print "%s:" i1];
                  gen0 k2])

and genFunApp k1 lst = 
  let rec genArgList n = 
    if n = 0 then []
    else genArgList (n - 1) @ [AD.M (print "[rbp - %d]" (8 * (n + 1)))]
  in
  let lst = List.map AD.fromMV lst in
  match k1 with
  | Reg _ -> 
    List.concat 
      [moveList lst (genArgList (List.length lst));
       [AD.op2  "shr" (v2s k1) "1";
        AD._mov (AD.M "[rbp - 8]") (AD.M (print "[%s + 8]" (v2s k1)));
        AD.op2  "movshr" "r14" (print "[%s + 16]" (v2s k1));
        AD._mov (AD.R "rsp") (AD.R "rbp");
        AD.jmp  "r14"]]
  | CloRef (Reg 15, _, f) ->
    List.concat
      [moveList lst (genArgList (List.length lst));
       [AD._mov (AD.M "[rbp - 8]") (AD.R "r15");
        AD._mov (AD.R "rsp") (AD.R "rbp");
        AD.jmp  (print "fn%d" f)]]
  | CloRef (s, _, f) ->
    List.concat
      [[AD.mov  (StkVar (-1)) s];
       moveList lst (genArgList (List.length lst)); (* will overwrite s *)
       [AD._mov (AD.R "rsp") (AD.R "rbp");
        AD.jmp  (print "fn%d" f)]]
  | _ -> failwith "genFunApp: invalid caller"

and genTuple k lst rmap = 
  if List.length lst = 0 then [AD.mov k (Imm 0)]
  else 
    genExternCall "make_tuple" (Imm (List.length lst) :: lst) rmap k true;
 
and gen0 = function
  | MPrimOp1 (rm, id, dst, p1, k) -> 
    List.concat [genPrimOp1 id dst p1 rm; gen0 k]
  | MPrimOp2 (rm, id, dst, p1, p2, k) ->
    List.concat [genPrimOp2 id dst p1 p2 rm; gen0 k]
  | MCond (_, mv, k1, k2) ->
    genCond mv k1 k2
  | MFunApp (_, k1, lst) ->
    genFunApp k1 lst
  | MTuple (rm, dst, lst, k) ->
    List.concat [genTuple dst lst rm; gen0 k]
  | MCopy (_, dst, src, k) -> 
    (AD.mov dst src) :: gen0 k
  | MTerm -> ["jmp\t_ml_term"]

let genFuncBody = function
  | Proc (id, scnt, hcnt, argcnt, impl) ->
    let argcnt, scnt = argcnt + 1, scnt + 1 in (* the closure parameter *)
    let argDiff = 8 * (argcnt / 2 * 2 + 1) in
    let stkDiff = 8 * (scnt / 2 * 2 + 1) - argDiff in
    List.concat [[print "\nfn%d:" id; (* caller ensure that rbp=rsp *)
                  AD.op2 "sub" "rsp" (string_of_int argDiff);
                  (* ensure that gc will not touch uninitialized data *)
                  AD.optional
                    ((argcnt mod 2 = 0) && (hcnt > 0))
                    (AD.mov (StkVar (argcnt + 1)) (Imm 0))];
                 AD.optionalL
                   (hcnt > 0) 
                   (genExternCall "gc_check" [Imm hcnt] RM.empty (Reg 0) false);
                 [AD._mov (AD.R "r15") (AD.M "[rbp - 8]");
                  AD.op2 "sub" "rsp" (string_of_int stkDiff)];
                 gen0 impl]
  | Main (scnt, hcnt, impl) ->
    let stkDiff = 8 * ((scnt + 1) / 2 * 2 + 1) in
    List.concat [["\nmain:";
                  "mov\trbp, rsp";
                  "call\theap_init"];
                 AD.optionalL
                   (hcnt > 0)
                   (genExternCall "gc_check" [Imm hcnt] RM.empty (Reg 0) false);
                 [AD.op2 "sub" "rsp" (string_of_int stkDiff)];
                 gen0 impl;
                 ["\n_ml_term:";
                  "mov\trsp, rbp";
                  "mov\trax, 0";
                  "ret"]]

let repString str =
  let transStr s =
    let s0 = Scanf.unescaped s in
    String.concat ", " @@ 
    List.map (fun c -> string_of_int @@ int_of_char c) @@
    list_of_string s0
  in
  let header = print "0574%012x" (String.length str * 2) in
  let h0 = String.concat "" @@ Array.to_list @@ 
      (Array.init 8 
         (fun x -> String.concat "" ["0x"; 
                                     String.make 1 header.[14 - x * 2];
                                     String.make 1 header.[15 - x * 2];
                                     ", "])) in
  let ret = print "%s%s, 0" h0 (transStr str) in
  if String.length str mod 2 = 1 then ret else print "%s, 0" ret

let genDataSection strmap = 
  if StrMap.cardinal strmap = 0 then []
  else
    List.concat [["section .data\n"];
                 List.map (fun (str, id) ->
                     print "str%d\tdb\t%s" id (repString str))
                   (StrMap.bindings strmap)]

let mgen ((strmap, fix): RM.t Prog.t) = 
  List.filter (fun s -> not (s = "")) @@ 
  List.concat [["\n%macro\tmovmem 2";
                "mov    r14, %2";
                "mov    %1, r14";
                "%endmacro";
                "\n%macro\tmovshl 2";
                "mov    %1, %2";
                "shl    %1, 1";
                "%endmacro";
                "\n%macro\tmovshr 2";
                "mov    %1, %2";
                "shr    %1, 1";
                "%endmacro";
                "\n%macro\tsavetup 1";
                "mov    %1, r15";
                "shl    %1, 1";
                "add    %1, 1";
                "%endmacro"
               ];
               genDataSection strmap;
               ["\nsection .text\n";
                "global\tmain";
                "extern\tdisp, make_tuple, dispi, heap_init, gc_check, \
                 polyLT, equiv, match_failure"];
               List.concat @@ List.map genFuncBody fix;
              ]

