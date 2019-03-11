let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        while Lexer.scanner lexbuf != Parse.EOF do
            ()
        done;
        print_endline "OK: scanner completed successfully"
    with Lexer.Error -> Helpers.print_error lexbuf "Scanner"
