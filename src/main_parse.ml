
let _ =
    let lb = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lb in
        Weeder.wd_prog ast;
        print_endline "OK: parser completed successfully"
    with
    | Parse.Error -> Helpers.print_error lb "Parser"
    | e           -> Helpers.handle_error ~default:"Pretty-Printer" lb e
