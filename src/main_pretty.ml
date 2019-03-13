let _ =
    let lb = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lb in
        Pretty.pp_prog ast;
        print_endline "OK: pretty-printer completed successfully"
    with
    | Parse.Error -> Helpers.print_error lb "Parser"
    | e           -> Helpers.handle_error lb e
