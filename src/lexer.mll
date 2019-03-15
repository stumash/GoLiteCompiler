{
    (* Add to contents of generated lexer.ml *)

    open Parse

    let should_print_tokens = ref false

    (* mpt - Maybe Print Token *)
    let mpt s_format x =
        if not !should_print_tokens then () else
        Printf.printf s_format x

    let last_token = ref EOF
    let slt_returns_eof = ref false

    (* slt - Set Last Token (and return it, unless slt_returns_eof is true) *)
    let slt tok =
        last_token := tok;
        if !slt_returns_eof then EOF else tok

    (* https://golang.org/ref/spec#Semicolons (Rule 1 only) *)
    let is_stmt_end tok =
        match tok with
            | IDENT s                                 -> true
            | LIT_INT i                               -> true
            | LIT_FLOAT f                             -> true
            | LIT_BOOL b                              -> true
            | LIT_RUNE s                              -> true
            | LIT_STRING s                            -> true
            | LIT_RAW_STRING r                         -> true
            | RPAREN | RBRACK | RCURLY | INC | DEC    -> true
            | BREAK | CONTINUE | FALLTHROUGH | RETURN -> true
            | _                                       -> false

    (* String of Go Int To Int *)
    (* sgi - String repr. of Go Int *)
    (* soi - String repr. of OCaml Int *)
    let sgi2i sgi =
        let soi =
            let len = String.length sgi in
            if len >= 2 && sgi.[0] == '0' && sgi.[1] != 'X' && sgi.[1] != 'x' then
                "0o" ^ (String.sub sgi 1 (len-1))
            else sgi in
        int_of_string soi

    (* String of Go Float to Float *)
    (* Go float syntax is compatible with OCaml float syntax *)
    let sgf2f = float_of_string

    (* String of Go Boolean to Boolean *)
    (* Go boolean syntax is compatible with OCaml boolean syntax *)
    let sgb2b = bool_of_string

    (* ocamllex only manages pos_cnum automatically, need to do pos_lnum and pos_bol manually *)
    let incr_linenum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- {
            pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
            Lexing.pos_bol = pos.Lexing.pos_cnum
        }
}

(* pattern definitions *)

let digit = ['0'-'9']
let digits = digit digit*
let nzdigit = ['1'-'9'] (* non-zero digit *)
let octal_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let line_comment = '/''/'[^ '\n']*
let block_comment = '/''*' ( [^ '*'] | '*''*'* [^ '/'] )* '*''*'* '/'
let comment = line_comment | block_comment

let lit_decimal = '0' | nzdigit digit*
let lit_octal = '0' octal_digit*
let lit_hexadecimal = '0'['x''X'] hex_digit hex_digit*
let lit_int = lit_decimal | lit_octal | lit_hexadecimal

let expon = ['e''E'] ['+''-'] digit digit*
let lit_float =  digits '.' digit* expon? | digit* '.' digits expon?

let lit_bool = "true" | "false"

let escape_char = "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\\\"
let lit_rune = '\'' ([^ '\t' '\n' '\'' '\\' '\n'] | escape_char | "\\'" ) '\''
let lit_string = '"' ([^ '`' '\\' '"' '\n'] | escape_char | "\\\"")* '"'
let lit_raw_string = '`' ([^ '`' '\n'] | escape_char | "\\\`")* '`'


(* lexer *)

rule scanner = parse
(* keywords *)
  | "break" as s          { mpt "%s\n" s; slt BREAK }
  | "case" as s           { mpt "%s\n" s; slt CASE }
  | "chan" as s           { mpt "%s\n" s; slt CHAN }
  | "const" as s          { mpt "%s\n" s; slt CONST }
  | "continue" as s       { mpt "%s\n" s; slt CONTINUE }
  | "default" as s        { mpt "%s\n" s; slt DEFAULT }
  | "defer" as s          { mpt "%s\n" s; slt DEFER }
  | "else" as s           { mpt "%s\n" s; slt ELSE }
  | "fallthrough" as s    { mpt "%s\n" s; slt FALLTHROUGH }
  | "for" as s            { mpt "%s\n" s; slt FOR }
  | "func" as s           { mpt "%s\n" s; slt FUNC }
  | "go" as s             { mpt "%s\n" s; slt GO }
  | "goto" as s           { mpt "%s\n" s; slt GOTO }
  | "if" as s             { mpt "%s\n" s; slt IF }
  | "import" as s         { mpt "%s\n" s; slt IMPORT }
  | "interface" as s      { mpt "%s\n" s; slt INTERFACE }
  | "map" as s            { mpt "%s\n" s; slt MAP }
  | "package" as s        { mpt "%s\n" s; slt PACKAGE }
  | "range" as s          { mpt "%s\n" s; slt RANGE }
  | "return" as s         { mpt "%s\n" s; slt RETURN }
  | "select" as s         { mpt "%s\n" s; slt SELECT }
  | "struct" as s         { mpt "%s\n" s; slt STRUCT }
  | "switch" as s         { mpt "%s\n" s; slt SWITCH }
  | "type" as s           { mpt "%s\n" s; slt TYPE }
  | "var" as s            { mpt "%s\n" s; slt VAR }
  | "_"   as c            { mpt "%s\n" "blank_identifier"; slt (IDENT (String.make 1 c)) }
(* 'keyword' functions *)
  | "print" as s          { mpt "%s\n" s; slt PRINT }
  | "println" as s        { mpt "%s\n" s; slt PRINTLN }
  | "append" as s         { mpt "%s\n" s; slt APPEND }
  | "len" as s            { mpt "%s\n" s; slt LEN }
  | "cap" as s            { mpt "%s\n" s; slt CAP }
(* punctuation *)
  | "(" as c              { mpt "%c\n" c; slt LPAREN }
  | ")" as c              { mpt "%c\n" c; slt RPAREN }
  | "[" as c              { mpt "%c\n" c; slt LBRACK }
  | "]" as c              { mpt "%c\n" c; slt RBRACK }
  | "{" as c              { mpt "%c\n" c; slt LCURLY }
  | "}" as c              { mpt "%c\n" c; slt RCURLY }
  | ":" as c              { mpt "%c\n" c; slt COLON }
  | ";" as c              { mpt "%c\n" c; slt SEMICOLON }
  | "," as c              { mpt "%c\n" c; slt COMMA }
  | "." as c              { mpt "%c\n" c; slt DOT }
  | "..." as s            { mpt "%s\n" s; slt ELLIPSIS }
(* operators *)
  | "+" as c              { mpt "%c\n" c; slt PLUS } (* arithmetic *)
  | "-" as c              { mpt "%c\n" c; slt MINUS }
  | "*" as c              { mpt "%c\n" c; slt MULT }
  | "/" as c              { mpt "%c\n" c; slt DIV }
  | "%" as c              { mpt "%c\n" c; slt MOD }
  | "&" as c              { mpt "%c\n" c; slt BAND } (* bitwise *)
  | "|" as c              { mpt "%c\n" c; slt BOR }
  | "^" as c              { mpt "%c\n" c; slt XOR }
  | "<<" as s             { mpt "%s\n" s; slt LSHFT }
  | ">>" as s             { mpt "%s\n" s; slt RSHFT }
  | "&^" as s             { mpt "%s\n" s; slt NAND }
  | "+=" as s             { mpt "%s\n" s; slt PLUSEQ } (* arithmetic assignment *)
  | "-=" as s             { mpt "%s\n" s; slt MINUSEQ }
  | "*=" as s             { mpt "%s\n" s; slt MULTEQ }
  | "/=" as s             { mpt "%s\n" s; slt DIVEQ }
  | "%=" as s             { mpt "%s\n" s; slt MODEQ }
  | "++" as s             { mpt "%s\n" s; slt INC }
  | "--" as s             { mpt "%s\n" s; slt DEC }
  | "&=" as s             { mpt "%s\n" s; slt BANDEQ } (* bitwise assignment *)
  | "|=" as s             { mpt "%s\n" s; slt BOREQ }
  | "^=" as s             { mpt "%s\n" s; slt XOREQ }
  | "<<=" as s            { mpt "%s\n" s; slt LSHFTEQ }
  | ">>=" as s            { mpt "%s\n" s; slt RSHFTEQ }
  | "&^=" as s            { mpt "%s\n" s; slt NANDEQ }
  | "==" as s             { mpt "%s\n" s; slt EQ } (* comparison *)
  | "!=" as s             { mpt "%s\n" s; slt NEQ }
  | "<" as c              { mpt "%c\n" c; slt LT }
  | "<=" as s             { mpt "%s\n" s; slt LTEQ }
  | ">" as c              { mpt "%c\n" c; slt GT }
  | ">=" as s             { mpt "%s\n" s; slt GTEQ }
  | "!" as c              { mpt "%c\n" c; slt NOT }
  | "&&" as s             { mpt "%s\n" s; slt AND }
  | "||" as s             { mpt "%s\n" s; slt OR }
  | "<-" as s             { mpt "%s\n" s; slt CHASG } (* assignment *)
  | "=" as c              { mpt "%c\n" c; slt ASG }
  | ":=" as s             { mpt "%s\n" s; slt IASG }
(* parametrized *)
  | comment as s          { mpt "comment(%s)\n" s;    scanner lexbuf (* no comments for parser *) }
  | id as s               { mpt "identifier(%s)\n" s; slt (IDENT s) }
  | lit_int as s          { mpt "lit_int(%s)\n" s;    slt (LIT_INT (sgi2i s)) }
  | lit_float as s        { mpt "lit_float(%s)\n" s;  slt (LIT_FLOAT (sgf2f s)) }
  | lit_bool as s         { mpt "lit_bool(%s)\n" s;   slt (LIT_BOOL (sgb2b s)) }
  | lit_string as s       { mpt "lit_string(%s)\n" s; slt (LIT_STRING s) }
  | lit_raw_string as s   { mpt "lit_raw_string(%s)\n" s; slt (LIT_RAW_STRING s)}
  | lit_rune as s         { mpt "lit_rune(%s)\n" s;   slt (LIT_RUNE s) }
(* special *)
  | [' ' '\t']            { scanner lexbuf }
  | '\n'                  { incr_linenum lexbuf;
                            if is_stmt_end !last_token then
                              ( mpt "%s\n" ";" ; slt SEMICOLON )
                            else scanner lexbuf
                          }
  | eof                   { slt_returns_eof := true;
                            if !last_token != SEMICOLON then
                              ( mpt "%s\n" ";"; last_token := SEMICOLON; SEMICOLON )
                            else
                              ( mpt "%s\n" "EOF"; EOF )
                          }
  | _                     { raise Helpers.LexerError }


(*trailer
 * ---------- *)

{
    (* nothing *)
}
