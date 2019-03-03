open Parserexceptions

let _ =
    let lexbuf = Lexing.from_channel stdin in
    try Parse.program Lexer.scanner lexbuf; () with
    | Parse.Error               -> Helpers.print_error lexbuf "Parser"
    | ExpressionIsNotIdentifier -> Helpers.print_error lexbuf "Parser: exp is not ident"
