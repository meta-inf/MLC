open Ast
open Value
open Eval
open Parse.Parse_debug

let test str = eval (parse1 str) Table.empty (ref RefTable.empty) (fun x -> x)
