(* Print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let {pos_lnum; pos_cnum} = lexbuf.lex_curr_p in
    Printf.printf "%s Error at L%d,C%d: '%s'\n" compiler_stage pos_lnum pos_cnum (lexeme lexbuf))
