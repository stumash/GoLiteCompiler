open Parser_exceptions

let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        let _ = Parse.program Lexer.scanner lexbuf in
        ()
    with
    | Parse.Error               -> Helpers.print_error lexbuf "Parser"
    | ExpressionIsNotIdentifier -> Helpers.print_error lexbuf "Parser: exp is not ident"
