{
open Syntax
open Lexing
exception Lexer_error of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
	{ pos with pos_bol = lexbuf.lex_curr_pos;
			   pos_lnum = pos.pos_lnum + 1
	}
}

let digit = ['0'-'9']
let uchar = ['A'-'Z']
let lchar = ['a'-'z']
let idbody = uchar | lchar | digit | ['_']
let char_ = [^'\n' '"']

rule token = parse
| "(*" [^ '\n']* "*)"	{ token lexbuf }
| [' ' '\t']	{ token lexbuf }
| ['\n']	{ next_line lexbuf; token lexbuf }
| ';'	{ SEMICOLON }
| '('	{ LPAREN }  
| ')'	{ RPAREN }
| '['	{ LBRAC }
| ']'	{ RBRAC }
| ','	{ COMMA }
| '='	{ EQUAL }
| '|'	{ VERTBAR }
| "::"  { LISTCONS }
| "->"	{ ARROW }
| ":=" as op { OP0 op }
| ['<' '>'] idbody* as op	
	{ OP1 op }
| "=="
	{ OP1 "==" }
| ['$'] idbody* as op
        { OP2 op }
| ['+' '-'] idbody* as op	
	{ OP3 op }
| ['*' '/'] idbody* as op
	{ OP4 op }

| "\"" char_+ "\"" as str
	{ STRCONST (String.sub str 1 (String.length str - 2)) }

| "type"	{ TYPE }
| "of"	{ OF }
| "match"	{ MATCH }
| "with"	{ WITH }
| "as"	{ AS }
| "and"	{ AND }
| "let"	{ LET }
| "rec"	{ REC }
| "fun"	{ FUNC }
| "function"	{ FUNC }
| "in"	{ IN }
| "if"	{ IF }
| "then"	{ THEN }
| "else"	{ ELSE }
| "begin"	{ BEGIN }
| "end"	{ END }
| (lchar | '_') idbody* as id
	{ LID id }
| uchar idbody* as id
	{ UID id }
| ''' idbody* as id
	{ TID id }
| digit+
| "-" digit+ as num
	{ INTNUM (int_of_string num) }

| "." digit+
| digit+ "." digit* 
| "+" digit+ "." digit* 
| "-" digit+ "." digit* as num
	{ FLOATNUM (float_of_string num) }

| eof	{ EOF }
