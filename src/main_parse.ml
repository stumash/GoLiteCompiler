module H = Helpers

let _ =
    let lb = Lexing.from_channel stdin in
    try
        let _ = Parse.program Lexer.scanner lb in
        print_endline "OK: parser completed successfully"
    with
    | Parse.Error                 -> H.print_error lb "Parser"
    | H.ExpressionIsNotIdentifier -> H.print_error lb "Parser: exp is not ident"
    | H.VarDecIdsLenNeqExpsLen    -> H.print_error lb "Parser: var. decl. LHS size unequal RHS size"
    | H.VarDecNeedsTypeOrInit     -> H.print_error lb "Parser: var. decl. needs type or initializer"
    | H.NotSimpleStatement        -> H.print_error lb "Parser: statement is not a 'simple statement'"
    | H.MultAsgCannotShorthand    -> H.print_error lb "Parser: multiple assignment must use '='"
    | Lexer.Error                 -> H.print_error lb "Scanner"
