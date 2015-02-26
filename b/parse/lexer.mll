{
open Parser
open IdTable
}
let digit = ['0'-'9']
let idbeg = ['a'-'z' 'A'-'Z' '_' '=' '+' '-' '*' '/']
let idnorm = idbeg | ['0'-'9' '\'']
let char_ = [^'\n' '"']

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }

  | digit+
  | "-" digit+ as num
        { INTNUM (int_of_string num) }

  | "." digit+
  | digit+ "." digit* 
  | "+" digit+ "." digit* 
  | "-" digit+ "." digit* as num
        { FLOATNUM (float_of_string num) }
  
  | "\"" char_+ "\"" as str
        { STRCONST (String.sub str 1 (String.length str - 2)) }

  | "#t" { BOOLNUM true }
  | "#f" { BOOLNUM false }
  
  | "(" { LBRACKET }
  | ")" { RBRACKET }

  | "fun" { LAMBDA }
  | "tuple" { TUPLE }
  | "letrec" { LETREC }
  | "let" { LET }
  | "cond" { COND }

  | idbeg idnorm* as str
        { IDENTIFIER (IdTable.findOrAdd str) }

  | eof	{ raise End_of_file }
