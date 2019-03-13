(* * 
 * ERROR HELPERS
 *--------------------*)

(* print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let {pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
    let cnum = pos_cnum - pos_bol - ((lexeme_end lexbuf) - (lexeme_start lexbuf)) in
    Printf.printf "Error: '%s' at L%d,C%d: '%s'\n" (lexeme lexbuf) pos_lnum cnum compiler_stage);
    exit 1; ()

exception ExpressionIsNotIdentifier
exception VarDecNeedsTypeOrInit
exception VarDecIdsLenNeqExpsLen
exception NotSimpleStatement
