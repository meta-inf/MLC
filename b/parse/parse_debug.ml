let parse channel =
  Parser.expr Lexer.token (Lexing.from_channel channel)

let parse1 str =
  Parser.expr Lexer.token (Lexing.from_string str)

