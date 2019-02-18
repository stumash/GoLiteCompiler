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
let nzdigit = ['1'-'9'] (* non-zero digit *)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = '/''*' ( [^ '*'] | '*''*'* [^ '/'] )* '*''*'* '/'

(* TODO: literals *)
let litint = digit*
let litfloat = digit*
let litbool = "true" | "false"
let litstring = '"' "TODO"* '"'
let litrune = '\'' "TODO"* '\''

rule scanner = parse
(* keywords *)
  | "break" as s          { mpt "%s\n" s; BREAK }
  | "case" as s           { mpt "%s\n" s; CASE }
  | "chan" as s           { mpt "%s\n" s; CHAN }
  | "const" as s          { mpt "%s\n" s; CONST }
  | "continue" as s       { mpt "%s\n" s; CONTINUE }
  | "default" as s        { mpt "%s\n" s; DEFAULT }
  | "defer" as s          { mpt "%s\n" s; DEFER }
  | "else" as s           { mpt "%s\n" s; ELSE }
  | "fallthrough" as s    { mpt "%s\n" s; FALLTHROUGH }
  | "for" as s            { mpt "%s\n" s; FOR }
  | "func" as s           { mpt "%s\n" s; FUNC }
  | "go" as s             { mpt "%s\n" s; GO }
  | "goto" as s           { mpt "%s\n" s; GOTO }
  | "if" as s             { mpt "%s\n" s; IF }
  | "import" as s         { mpt "%s\n" s; IMPORT }
  | "interface" as s      { mpt "%s\n" s; INTERFACE }
  | "map" as s            { mpt "%s\n" s; MAP }
  | "package" as s        { mpt "%s\n" s; PACKAGE }
  | "range" as s          { mpt "%s\n" s; RANGE }
  | "return" as s         { mpt "%s\n" s; RETURN }
  | "select" as s         { mpt "%s\n" s; SELECT }
  | "struct" as s         { mpt "%s\n" s; STRUCT }
  | "switch" as s         { mpt "%s\n" s; SWITCH }
  | "type" as s           { mpt "%s\n" s; TYPE }
  | "var" as s            { mpt "%s\n" s; VAR }
  | "_"                   { mpt "%s\n" "blank_identifier"; BLANKID }
(* 'keyword' functions *)
  | "print" as s          { mpt "%s\n" s; PRINT }
  | "println" as s        { mpt "%s\n" s; PRINTLN }
  | "append" as s         { mpt "%s\n" s; APPEND }
  | "len" as s            { mpt "%s\n" s; LEN }
  | "cap" as s            { mpt "%s\n" s; CAP }
(* punctuation *)
  | "(" as c              { mpt "%c\n" c; LPAREN }
  | ")" as c              { mpt "%c\n" c; RPAREN }
  | "[" as c              { mpt "%c\n" c; LBRACK }
  | "]" as c              { mpt "%c\n" c; RBRACK }
  | "{" as c              { mpt "%c\n" c; LCURLY }
  | "}" as c              { mpt "%c\n" c; RCURLY }
  | ":" as c              { mpt "%c\n" c; COLON }
  | ";" as c              { mpt "%c\n" c; SEMICOLON }
  | "," as c              { mpt "%c\n" c; COMMA }
  | "." as c              { mpt "%c\n" c; DOT }
  | "..." as s            { mpt "%s\n" s; ELLIPSIS }
(* operators *)
  | "+" as c              { mpt "%c\n" c; PLUS } (* arithmetic *)
  | "-" as c              { mpt "%c\n" c; MINUS }
  | "*" as c              { mpt "%c\n" c; MULT }
  | "/" as c              { mpt "%c\n" c; DIV }
  | "%" as c              { mpt "%c\n" c; MOD }
  | "&" as c              { mpt "%c\n" c; BAND } (* bitwise *)
  | "|" as c              { mpt "%c\n" c; BOR }
  | "^" as c              { mpt "%c\n" c; XOR }
  | "<<" as s             { mpt "%s\n" s; LSHFT }
  | ">>" as s             { mpt "%s\n" s; RSHFT }
  | "&^" as s             { mpt "%s\n" s; NAND }
  | "+=" as s             { mpt "%s\n" s; PLUSEQ } (* arithmetic assignment *)
  | "-=" as s             { mpt "%s\n" s; MINUSEQ }
  | "*=" as s             { mpt "%s\n" s; MULTEQ }
  | "/=" as s             { mpt "%s\n" s; DIVEQ }
  | "%=" as s             { mpt "%s\n" s; MODEQ }
  | "++" as s             { mpt "%s\n" s; INC }
  | "--" as s             { mpt "%s\n" s; DEC }
  | "&=" as s             { mpt "%s\n" s; BANDEQ } (* bitwise assignment *)
  | "|=" as s             { mpt "%s\n" s; BOREQ }
  | "^=" as s             { mpt "%s\n" s; XOREQ }
  | "<<=" as s            { mpt "%s\n" s; LSHFTEQ }
  | ">>=" as s            { mpt "%s\n" s; RSHFTEQ }
  | "&^=" as s            { mpt "%s\n" s; NANDEQ }
  | "==" as s             { mpt "%s\n" s; EQ } (* comparison *)
  | "!=" as s             { mpt "%s\n" s; NEQ }
  | "<" as c              { mpt "%c\n" c; LT }
  | "<=" as s             { mpt "%s\n" s; LTEQ }
  | ">" as c              { mpt "%c\n" c; GT }
  | ">=" as s             { mpt "%s\n" s; GTEQ }
  | "!" as c              { mpt "%c\n" c; NOT }
  | "<-" as s             { mpt "%s\n" s; CHASG } (* assignment *)
  | "=" as c              { mpt "%c\n" c; ASG }
  | ":=" as s             { mpt "%s\n" s; IASG }
(* parametrized *)
  | comment as s          { mpt "comment(%s)\n" s; COMMENT s }
  | id as s               { mpt "identifier(%s)\n" s; IDENT s }
  | litint as s           { mpt "litint(%s)\n" s; LITINT (int_of_string s) }
  | litfloat as s         { mpt "litfloat(%s)\n" s; LITFLOAT (float_of_string s) }
  | litbool as s          { mpt "litbool(%s)\n" s; LITBOOL (bool_of_string s) }
  | litstring as s        { mpt "litstring(%s)\n" s; LITSTRING s }
  | litrune as s          { mpt "litrune(%s)\n" s; LITRUNE s }
(* special *)
  | [' ' '\t']            { scanner lexbuf (* ignore whitespace *) }
  | ['\n']                { mpt "%s\n" "semicolon"; SEMICOLON }
  | _                     { print_endline "\nERROR"; exit 1 }
  | eof                   { mpt "%s\n" "EOF"; EOF}

(*need to add operators and other misc symbols to the grammar above *)

(*trailer
 * ---------- *)
{
    (* nothing *)
}
