(* header
 * ---------- *)
{
    open Parse

    let should_print_tokens = ref false

    (* mpt - Maybe Print Token *)
    let mpt s_format s =
        if not !should_print_tokens then () else
        Printf.printf s_format s
}

(* body
 * ---------- *)

let digit = ['0'-'9']
let nz_digit = ['1'-'9'] (* non-zero digit *)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let in_comment = ( [^ '*'] | '*'[^ '/'] )*

rule scanner = parse
  | "break" as s            { mpt "%s\n" s; BREAK }
  | "case" as s             { mpt "%s\n" s; CASE }
  | "chan" as s             { mpt "%s\n" s; CHAN }
  | "const" as s            { mpt "%s\n" s; CONST }
  | "continue" as s         { mpt "%s\n" s; CONTINUE }
  | "default" as s          { mpt "%s\n" s; DEFAULT }
  | "defer" as s            { mpt "%s\n" s; DEFER }
  | "else" as s             { mpt "%s\n" s; ELSE }
  | "fallthrough" as s      { mpt "%s\n" s; FALLTHROUGH }
  | "for" as s              { mpt "%s\n" s; FOR }
  | "func" as s             { mpt "%s\n" s; FUNC }
  | "go" as s               { mpt "%s\n" s; GO }
  | "goto" as s             { mpt "%s\n" s; GOTO }
  | "if" as s               { mpt "%s\n" s; IF }
  | "import" as s           { mpt "%s\n" s; IMPORT }
  | "interface" as s        { mpt "%s\n" s; INTERFACE }
  | "map" as s              { mpt "%s\n" s; MAP }
  | "package" as s          { mpt "%s\n" s; PACKAGE }
  | "range" as s            { mpt "%s\n" s; RANGE }
  | "return" as s           { mpt "%s\n" s; RETURN }
  | "select" as s           { mpt "%s\n" s; SELECT }
  | "struct" as s           { mpt "%s\n" s; STRUCT }
  | "switch" as s           { mpt "%s\n" s; SWITCH }
  | "type" as s             { mpt "%s\n" s; TYPE }
  | "var" as s              { mpt "%s\n" s; VAR }
  | "print" as s            { mpt "%s\n" s; PRINT }
  | "println" as s          { mpt "%s\n" s; PRINTLN }
  | "append" as s           { mpt "%s\n" s; APPEND }
  | "len" as s              { mpt "%s\n" s; LEN }
  | "cap" as s              { mpt "%s\n" s; CAP }
  | "/*"in_comment"*/" as s { mpt "comment(%s)\n" s; COMMENT s }
  | id as s                 { mpt "identifier(%s)\n" s; ID s }
  | '_'                     { mpt "%s\n" "blank_identifier"; BLANKID }
  | [' ' '\t']              { scanner lexbuf (* ignore whitespace *) }
  | ['\n']                  { mpt "%s\n" "semicolon"; SEMICOLON }
  | eof                     { mpt "%s\n" "OK"; EOF}


(*trailer
 * ---------- *)
{
    (* nothing *)
}
