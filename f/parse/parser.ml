open Lexing

exception Parse_error of string

let printPosition lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse1 str =
  let lexbuf = Lexing.from_string str in
  try
    Syntax.prog Lexer.token lexbuf
  with 
    Syntax.Error ->
    raise @@ Parse_error (Printf.sprintf "%s: syntax error" (printPosition lexbuf))
  | _ -> 
    raise @@ Parse_error (Printf.sprintf "%s: syntax error" (printPosition lexbuf))

let readAll = 
  let rec read0 ret = 
    try let u = read_line () in read0 (u :: ret)
    with End_of_file -> ret
  in fun () -> String.concat "\n" (List.rev (read0 []))
