(* * 
 * ERROR HELPERS
 *--------------------*)

(* print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let {pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
    let cnum = pos_cnum - pos_bol - ((lexeme_end lexbuf) - (lexeme_start lexbuf)) in
    Printf.printf "%s Error at L%d,C%d: '%s'\n" compiler_stage pos_lnum cnum (lexeme lexbuf))
