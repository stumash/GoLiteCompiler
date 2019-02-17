(* header *)
{
    open Parse
}

(* body *)

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9''_']*
let blank = '_'

(*need to decide on how to split our scanner into parts or should we or can we *)
rule scanner = parse
  | "break" as s       { print_string s; sc_place lexbuf }
  | "case" as s        { print_string s }
  | "chan" as s        { print_string s }
  | "const" as s       { print_string s }
  | "continue" as s    { print_string s; sc_place lexbuf }
  | "default" as s     { print_string s }
  | "defer" as s       { print_string s }
  | "else" as s        { print_string s }
  | "fallthrough" as s { print_string s; sc_place lexbuf }
  | "for" as s         { print_string s }
  | "func" as s        { print_string s }
  | "go" as s          { print_string s }
  | "goto" as s        { print_string s }
  | "if" as s          { print_string s }
  | "import" as s      { print_string s }
  | "interface" as s   { print_string s }
  | "map" as s         { print_string s }
  | "package" as s     { print_string s }
  | "range" as s       { print_string s }
  | "return" as s      { print_string s; sc_place lexbuf }
  | "select" as s      { print_string s }
  | "struct" as s      { print_string s }
  | "switch" as s      { print_string s }
  | "type" as s        { print_string s }
  | "var" as s         { print_string s }
  | "print" as s       { print_string s }
  | "println" as s     { print_string s }
  | "append" as s      { print_string s }
  | "len" as s         { print_string s }
  | "cap" as s         { print_string s }
  | "/*"               { print_string "Comment"; comment lexbuf }
  | id                 { print_string "identifier "; sc_place lexbuf }
  | blank              { print_string "blank_identifier"; sc_place lexbuf }
  | [' ' '\t']         { scanner lexbuf }
  | _ as s             { print_char s; } (* Ignore spaces *)
  | eof                { print_string "OK"; exit 0 }

and comment = parse
  | "*/" { scanner lexbuf }
  | _    { comment lexbuf }

and sc_place = parse
  | [' ' '\t'] { sc_place lexbuf }
  | ['\n']     { print_string ";";(*Somehow return a semicolon *) scanner lexbuf }
  | _          { scanner lexbuf }


(*trailer *)
{

}
