module H = Helpers

let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lexbuf in
        Pretty.pp_prog ast;
        print_endline "OK: pretty-printer completed successfully"
    with
    | Parse.Error                 -> H.print_error lexbuf "Parser"
    | H.ExpressionIsNotIdentifier -> H.print_error lexbuf "Parser: exp is not ident"
    | H.VarDecIdsLenNeqExpsLen    -> H.print_error lexbuf "Parser: variable declaration LHS size not equals RHS size"
    | H.VarDecNeedsTypeOrInit     -> H.print_error lexbuf "Parser: variable declaration needs type or initializer"
    | H.NotSimpleStatement        -> H.print_error lexbuf "Parser: statement is not a 'simple statement'"
    | Lexer.Error                 -> H.print_error lexbuf "Scanner"
    | _                           -> H.print_error lexbuf "Pretty printer"

