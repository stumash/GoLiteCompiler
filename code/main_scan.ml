(* main *)
let _ =
    let lexbuf = Lexing.from_channel stdin in
    Lexer.should_print_tokens := true;
    while Lexer.scanner lexbuf != EOF do
        ()
    done
