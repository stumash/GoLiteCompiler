(* main *)
let _ =
    let lexbuf = Lexing.from_channel stdin in
    while Lexer.scanner lexbuf != EOF do
        ()
    done;
    print_endline "\nscanner completed successfully"
