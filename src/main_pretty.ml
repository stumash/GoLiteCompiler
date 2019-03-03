let _ =
    let lexbuf = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lexbuf in
        Pretty.print_program ast
    with
    | Parse.Error -> Helpers.print_error lexbuf "Parser"
    | _ -> Helpers.print_error lexbuf "Pretty printer"
