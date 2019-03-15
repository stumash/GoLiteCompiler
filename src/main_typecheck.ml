let () =
    let lb = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lb in
        Typecheck.type_check_prog ast;
        print_endline "OK: typechecker completed successfully"
    with
    | Parse.Error -> Helpers.print_error lb "Parser"
    | e           -> Helpers.handle_error lb e ~default:"TypeCheckerDefault"
