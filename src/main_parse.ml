open Parser_exceptions

let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        let _ = Parse.program Lexer.scanner lexbuf in
        print_endline "OK: parser completed successfully"
    with
    | Parse.Error               -> Helpers.print_error lexbuf "Parser"
    | ExpressionIsNotIdentifier -> Helpers.print_error lexbuf "Parser: exp is not ident"
    | VarDecIdsLenNeqExpsLen    -> Helpers.print_error lexbuf "Parser: variable declaration LHS size not equals RHS size"
    | VarDecNeedsTypeOrInit     -> Helpers.print_error lexbuf "Parser: variable declaration needs type or initializer"
    | Lexer.Error               -> Helpers.print_error lexbuf "Scanner"
