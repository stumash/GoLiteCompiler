let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        while Lexer.scanner lexbuf != EOF do
            ()
        done;
        print_endline "\nscanner completed successfully"
    with Lexer.Error -> Helpers.print_error lexbuf "Scanner"
