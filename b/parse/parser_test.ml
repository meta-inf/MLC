let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let ret = Parser.expr Lexer.token lexbuf
      in begin
        Printer.printAst ret
      end
    done
  with End_of_file -> exit 0
