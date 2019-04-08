{
    (* Add to contents of generated lexer.ml *)

    open Parse
    open Helpers

    let should_print_tokens = ref false

    (* mpt - Maybe Print Token *)
    let mpt s_format x lexbuf =
        if not !should_print_tokens then () else
        let l,c = get_pos lexbuf in
        Printf.printf "%d,%d    " l c;
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
            | IDENT s                                         -> true
            | LIT_INT i                                       -> true
            | LIT_FLOAT f                                     -> true
            | LIT_BOOL b                                      -> true
            | LIT_RUNE s                                      -> true
            | LIT_STRING s                                    -> true
            | LIT_RAW_STRING r                                -> true
            | RPAREN _ | RBRACK _ | RCURLY _ | INC _ | DEC _  -> true
            | BREAK _ | CONTINUE _ | FALLTHROUGH _ | RETURN _ -> true
            | _                                               -> false
    
    let is_semicolon tok =
      match tok with
      | SEMICOLON _ -> true
      | _           -> false

    (* String of Go Int To Int *)
    (* sgi - String repr. of Go Int *)
    (* soi - String repr. of OCaml Int *)
    let sgi2i sgi =
        let soi =
            let len = String.length sgi in
            if len >= 2 && sgi.[0] = '0' && sgi.[1] <> 'X' && sgi.[1] <> 'x' then
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
let lit_raw_string = '`' ([^ '`' '\n'] | escape_char | "\\`")* '`'


(* lexer *)

rule scanner = parse
(* keywords *)
  | "break" as s          { mpt "%s\n" s lexbuf; slt ((BREAK (get_pos lexbuf))) }
  | "case" as s           { mpt "%s\n" s lexbuf; slt (CASE (get_pos lexbuf)) }
  | "chan" as s           { mpt "%s\n" s lexbuf; slt (CHAN (get_pos lexbuf)) }
  | "const" as s          { mpt "%s\n" s lexbuf; slt (CONST (get_pos lexbuf)) }
  | "continue" as s       { mpt "%s\n" s lexbuf; slt (CONTINUE (get_pos lexbuf)) }
  | "default" as s        { mpt "%s\n" s lexbuf; slt (DEFAULT (get_pos lexbuf)) }
  | "defer" as s          { mpt "%s\n" s lexbuf; slt (DEFER (get_pos lexbuf)) }
  | "else" as s           { mpt "%s\n" s lexbuf; slt (ELSE (get_pos lexbuf)) }
  | "fallthrough" as s    { mpt "%s\n" s lexbuf; slt (FALLTHROUGH (get_pos lexbuf)) }
  | "for" as s            { mpt "%s\n" s lexbuf; slt (FOR (get_pos lexbuf)) }
  | "func" as s           { mpt "%s\n" s lexbuf; slt (FUNC (get_pos lexbuf)) }
  | "go" as s             { mpt "%s\n" s lexbuf; slt (GO (get_pos lexbuf)) }
  | "goto" as s           { mpt "%s\n" s lexbuf; slt (GOTO (get_pos lexbuf)) }
  | "if" as s             { mpt "%s\n" s lexbuf; slt (IF (get_pos lexbuf)) }
  | "import" as s         { mpt "%s\n" s lexbuf; slt (IMPORT (get_pos lexbuf)) }
  | "interface" as s      { mpt "%s\n" s lexbuf; slt (INTERFACE (get_pos lexbuf)) }
  | "map" as s            { mpt "%s\n" s lexbuf; slt (MAP (get_pos lexbuf)) }
  | "package" as s        { mpt "%s\n" s lexbuf; slt (PACKAGE (get_pos lexbuf)) }
  | "range" as s          { mpt "%s\n" s lexbuf; slt (RANGE (get_pos lexbuf)) }
  | "return" as s         { mpt "%s\n" s lexbuf; slt (RETURN (get_pos lexbuf)) }
  | "select" as s         { mpt "%s\n" s lexbuf; slt (SELECT (get_pos lexbuf)) }
  | "struct" as s         { mpt "%s\n" s lexbuf; slt (STRUCT (get_pos lexbuf)) }
  | "switch" as s         { mpt "%s\n" s lexbuf; slt (SWITCH (get_pos lexbuf)) }
  | "type" as s           { mpt "%s\n" s lexbuf; slt (TYPE (get_pos lexbuf)) }
  | "var" as s            { mpt "%s\n" s lexbuf; slt (VAR (get_pos lexbuf)) }
  | "_"   as c            { mpt "%s\n" "blank_identifier" lexbuf; slt (IDENT ((String.make 1 c), (get_pos lexbuf))) }
(* 'keyword' functions *)
  | "print" as s          { mpt "%s\n" s lexbuf; slt (PRINT (get_pos lexbuf)) }
  | "println" as s        { mpt "%s\n" s lexbuf; slt (PRINTLN (get_pos lexbuf)) }
  | "append" as s         { mpt "%s\n" s lexbuf; slt (APPEND (get_pos lexbuf)) }
  | "len" as s            { mpt "%s\n" s lexbuf; slt (LEN (get_pos lexbuf)) }
  | "cap" as s            { mpt "%s\n" s lexbuf; slt (CAP (get_pos lexbuf)) }
(* punctuation *)
  | "(" as c              { mpt "%c\n" c lexbuf; slt (LPAREN (get_pos lexbuf)) }
  | ")" as c              { mpt "%c\n" c lexbuf; slt (RPAREN (get_pos lexbuf)) }
  | "[" as c              { mpt "%c\n" c lexbuf; slt (LBRACK (get_pos lexbuf)) }
  | "]" as c              { mpt "%c\n" c lexbuf; slt (RBRACK (get_pos lexbuf)) }
  | "{" as c              { mpt "%c\n" c lexbuf; slt (LCURLY (get_pos lexbuf)) }
  | "}" as c              { mpt "%c\n" c lexbuf; slt (RCURLY (get_pos lexbuf)) }
  | ":" as c              { mpt "%c\n" c lexbuf; slt (COLON (get_pos lexbuf)) }
  | ";" as c              { mpt "%c\n" c lexbuf; slt (SEMICOLON (get_pos lexbuf)) }
  | "," as c              { mpt "%c\n" c lexbuf; slt (COMMA (get_pos lexbuf)) }
  | "." as c              { mpt "%c\n" c lexbuf; slt (DOT (get_pos lexbuf)) }
  | "..." as s            { mpt "%s\n" s lexbuf; slt (ELLIPSIS (get_pos lexbuf)) }
(* operators *)
  | "+" as c              { mpt "%c\n" c lexbuf; slt (PLUS (get_pos lexbuf)) } (* arithmetic *)
  | "-" as c              { mpt "%c\n" c lexbuf; slt (MINUS (get_pos lexbuf)) }
  | "*" as c              { mpt "%c\n" c lexbuf; slt (MULT (get_pos lexbuf)) }
  | "/" as c              { mpt "%c\n" c lexbuf; slt (DIV (get_pos lexbuf)) }
  | "%" as c              { mpt "%c\n" c lexbuf; slt (MOD (get_pos lexbuf)) }
  | "&" as c              { mpt "%c\n" c lexbuf; slt (BAND (get_pos lexbuf)) } (* bitwise *)
  | "|" as c              { mpt "%c\n" c lexbuf; slt (BOR (get_pos lexbuf)) }
  | "^" as c              { mpt "%c\n" c lexbuf; slt (XOR (get_pos lexbuf)) }
  | "<<" as s             { mpt "%s\n" s lexbuf; slt (LSHFT (get_pos lexbuf)) }
  | ">>" as s             { mpt "%s\n" s lexbuf; slt (RSHFT (get_pos lexbuf)) }
  | "&^" as s             { mpt "%s\n" s lexbuf; slt (NAND (get_pos lexbuf)) }
  | "+=" as s             { mpt "%s\n" s lexbuf; slt (PLUSEQ (get_pos lexbuf)) } (* arithmetic assignment *)
  | "-=" as s             { mpt "%s\n" s lexbuf; slt (MINUSEQ (get_pos lexbuf)) }
  | "*=" as s             { mpt "%s\n" s lexbuf; slt (MULTEQ (get_pos lexbuf)) }
  | "/=" as s             { mpt "%s\n" s lexbuf; slt (DIVEQ (get_pos lexbuf)) }
  | "%=" as s             { mpt "%s\n" s lexbuf; slt (MODEQ (get_pos lexbuf)) }
  | "++" as s             { mpt "%s\n" s lexbuf; slt (INC (get_pos lexbuf)) }
  | "--" as s             { mpt "%s\n" s lexbuf; slt (DEC (get_pos lexbuf)) }
  | "&=" as s             { mpt "%s\n" s lexbuf; slt (BANDEQ (get_pos lexbuf)) } (* bitwise assignment *)
  | "|=" as s             { mpt "%s\n" s lexbuf; slt (BOREQ (get_pos lexbuf)) }
  | "^=" as s             { mpt "%s\n" s lexbuf; slt (XOREQ (get_pos lexbuf)) }
  | "<<=" as s            { mpt "%s\n" s lexbuf; slt (LSHFTEQ (get_pos lexbuf)) }
  | ">>=" as s            { mpt "%s\n" s lexbuf; slt (RSHFTEQ (get_pos lexbuf)) }
  | "&^=" as s            { mpt "%s\n" s lexbuf; slt (NANDEQ (get_pos lexbuf)) }
  | "==" as s             { mpt "%s\n" s lexbuf; slt (EQ (get_pos lexbuf)) } (* comparison *)
  | "!=" as s             { mpt "%s\n" s lexbuf; slt (NEQ (get_pos lexbuf)) }
  | "<" as c              { mpt "%c\n" c lexbuf; slt (LT (get_pos lexbuf)) }
  | "<=" as s             { mpt "%s\n" s lexbuf; slt (LTEQ (get_pos lexbuf)) }
  | ">" as c              { mpt "%c\n" c lexbuf; slt (GT (get_pos lexbuf)) }
  | ">=" as s             { mpt "%s\n" s lexbuf; slt (GTEQ (get_pos lexbuf)) }
  | "!" as c              { mpt "%c\n" c lexbuf; slt (NOT (get_pos lexbuf)) }
  | "&&" as s             { mpt "%s\n" s lexbuf; slt (AND (get_pos lexbuf)) }
  | "||" as s             { mpt "%s\n" s lexbuf; slt (OR (get_pos lexbuf)) }
  | "<-" as s             { mpt "%s\n" s lexbuf; slt (CHASG (get_pos lexbuf)) } (* assignment *)
  | "=" as c              { mpt "%c\n" c lexbuf; slt (ASG (get_pos lexbuf)) }
  | ":=" as s             { mpt "%s\n" s lexbuf; slt (IASG (get_pos lexbuf)) }
(* parametrized *)
  | comment as s          { mpt "comment(%s)\n" s lexbuf;    scanner lexbuf (* no comments for parser *) }
  | id as s               { mpt "identifier(%s)\n" s lexbuf; slt (IDENT (s, (get_pos lexbuf))) }
  | lit_int as s          { mpt "lit_int(%s)\n" s lexbuf;    slt (LIT_INT ((sgi2i s), (get_pos lexbuf))) }
  | lit_float as s        { mpt "lit_float(%s)\n" s lexbuf;  slt (LIT_FLOAT ((sgf2f s), (get_pos lexbuf))) }
  | lit_bool as s         { mpt "lit_bool(%s)\n" s lexbuf;   slt (LIT_BOOL ((sgb2b s), (get_pos lexbuf))) }
  | lit_string as s       { mpt "lit_string(%s)\n" s lexbuf; slt (LIT_STRING (s, (get_pos lexbuf))) }
  | lit_raw_string as s   { mpt "lit_raw_string(%s)\n" s lexbuf; slt (LIT_RAW_STRING (s, (get_pos lexbuf)))}
  | lit_rune as s         { mpt "lit_rune(%s)\n" s lexbuf;   slt (LIT_RUNE (s, (get_pos lexbuf))) }
(* special *)
  | [' ' '\t']            { scanner lexbuf }
  | '\n' | '\r' | "\r\n"  { incr_linenum lexbuf;
                            if is_stmt_end !last_token then
                              ( mpt "%s\n" ";" lexbuf; slt (SEMICOLON (get_pos lexbuf)) )
                            else scanner lexbuf
                          }
  | eof                   { slt_returns_eof := true;
                            if not (is_semicolon !last_token) then
                              ( mpt "%s\n" ";" lexbuf; last_token := (SEMICOLON (get_pos lexbuf)); (SEMICOLON (get_pos lexbuf)) )
                            else
                              ( mpt "%s\n" "EOF" lexbuf; EOF )
                          }
  | _                     { raise Helpers.LexerError }


(*trailer
 * ---------- *)

{
    (* nothing *)
}
