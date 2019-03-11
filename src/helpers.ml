(* * 
 * LEXER ERROR HELPERS
 *--------------------*)

(* print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let {pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
    let cnum = pos_cnum - pos_bol in
    Printf.printf "%s Error at L%d,C%d: '%s'\n" compiler_stage pos_lnum cnum (lexeme lexbuf))

(* * 
 * PRETTY PRINTER HELPERS
 *-----------------------*)

(* pretty print a list of xs, comma-separated, using a given pretty printer for type x *)
let pp_comma_separated_xs xs pp_x =
    let f i x = pp_x x; if i != (List.length xs)-1 then print_string ", " else () in
    List.iteri f xs

(* * 
 * GENERAL HELPERS
 *----------------*)

let ifsome o f =
    match o with
    | Some a -> f a
    | _      -> ()
