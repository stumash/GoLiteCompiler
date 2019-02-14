
(*Header *)
{

open Lexing 
open Saparse

let num_lines = ref 0
let num_chars = ref 0
}

let digit = ['0'-'9']
(*Pattern Body *)
rule count = parse
| '\n' { incr num_lines; incr num_chars; count lexbuf}
| digit+ {print_string (Lexing.lexeme lexbuf) }
| _ { print_string (Lexing.lexeme lexbuf); incr num_chars; count lexbuf }
| eof { () }

(*trailer *)

