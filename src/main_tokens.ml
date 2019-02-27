let _ =
    let lexbuf = Lexing.from_channel stdin in
    Lexer.should_print_tokens := true;
    try
        while Lexer.scanner lexbuf != Parse.EOF do
            ()
        done;
        print_endline "\ntokenizer completed successfully"
    with Lexer.Error -> Helpers.print_error lexbuf "Scanner"
