
(*Header *)
{

open Lexing 
open Saparse

let num_lines = ref 0
let num_chars = ref 0
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9''_']*
let blank = '_'


(*Pattern Body *)
(*
rule count = parse
| '\n' { incr num_lines; incr num_chars; count lexbuf}
| digit+ {print_string (Lexing.lexeme lexbuf) }
| _ { print_string (Lexing.lexeme lexbuf); incr num_chars; count lexbuf }
| eof { () }
*)

(*need to decide on how to split our scanner into parts or should we or can we *)
rule scanner = parse 
  | "break" as c {print_string c; sc_place lexbuf }
  | "case" as c {print_string c }
  | "chan" as c {print_string c }
  | "const" as c {print_string c }
  | "continue" as c {print_string c; sc_place lexbuf }
  | "default" as c {print_string c }
  | "defer" as c {print_string c }
  | "else" as c {print_string c }
  | "fallthrough" as c {print_string c; sc_place lexbuf}
  | "for" as c {print_string c }
  | "func" as c {print_string c }
  | "go" as c {print_string c }
  | "goto" as c {print_string c }
  | "if" as c {print_string c }
  | "import" as c {print_string c }
  | "interface" as c {print_string c }
  | "map" as c {print_string c }
  | "package" as c {print_string c }
  | "range" as c {print_string c }
  | "return" as c {print_string c; sc_place lexbuf }
  | "select" as c {print_string c }
  | "struct" as c {print_string c }
  | "switch" as c {print_string c }
  | "type" as c {print_string c }
  | "var" as c {print_string c }
  | "print" as c {print_string c }
  | "println" as c {print_string c }
  | "append" as c {print_string c }
  | "len" as c {print_string c }
  | "cap" as c {print_string c }
  | "/*" {print_string "Comment"; comment lexbuf}
  | id {print_string "identifier "; sc_place lexbuf}
  | blank {print_string "blank_identifier"; sc_place lexbuf}
  | [' ' '\t'] {scanner lexbuf}
  | _ as c {print_char c; } (* Ignore spaces *)
  | eof {print_string "OK"; exit 0}
and comment = parse 
  | "*/" {scanner lexbuf}
  | _ {comment lexbuf}
and sc_place = parse 
  | [' ' '\t'] {sc_place lexbuf}
  | ['\n']  {print_string ";";(*Somehow return a semicolon *) scanner lexbuf}
  | _   {scanner lexbuf}


(*trailer *)


