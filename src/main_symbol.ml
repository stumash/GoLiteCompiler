let () =
    let lb = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lb in
        try
            Typecheck.type_check_prog ast;
            Cactus_stack.print_cs_node Typecheck.root_context;
            print_endline "OK: printed symbol table"
        with
        | TypeCheckError s ->
            Cactus_stack.print_cs_node Typecheck.root_context;
            print_endline "OK: printed symbol table"
    with
    | Parse.Error -> Helpers.print_error lb "Parser"
    | e           -> Helpers.handle_error lb e ~default:"TypeCheckerDefault"
