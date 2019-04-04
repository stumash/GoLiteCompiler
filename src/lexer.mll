{
    (* Add to contents of generated lexer.ml *)

    open Parse
    open Helpers

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
let lit_raw_string = '`' ([^ '`' '\n'] | escape_char | "\\\`")* '`'


(* lexer *)

rule scanner = parse
(* keywords *)
  | "break" as s          { mpt "%s\n" s; slt ((BREAK (get_pos lexbuf))) }
  | "case" as s           { mpt "%s\n" s; slt (CASE (get_pos lexbuf)) }
  | "chan" as s           { mpt "%s\n" s; slt (CHAN (get_pos lexbuf)) }
  | "const" as s          { mpt "%s\n" s; slt (CONST (get_pos lexbuf)) }
  | "continue" as s       { mpt "%s\n" s; slt (CONTINUE (get_pos lexbuf)) }
  | "default" as s        { mpt "%s\n" s; slt (DEFAULT (get_pos lexbuf)) }
  | "defer" as s          { mpt "%s\n" s; slt (DEFER (get_pos lexbuf)) }
  | "else" as s           { mpt "%s\n" s; slt (ELSE (get_pos lexbuf)) }
  | "fallthrough" as s    { mpt "%s\n" s; slt (FALLTHROUGH (get_pos lexbuf)) }
  | "for" as s            { mpt "%s\n" s; slt (FOR (get_pos lexbuf)) }
  | "func" as s           { mpt "%s\n" s; slt (FUNC (get_pos lexbuf)) }
  | "go" as s             { mpt "%s\n" s; slt (GO (get_pos lexbuf)) }
  | "goto" as s           { mpt "%s\n" s; slt (GOTO (get_pos lexbuf)) }
  | "if" as s             { mpt "%s\n" s; slt (IF (get_pos lexbuf)) }
  | "import" as s         { mpt "%s\n" s; slt (IMPORT (get_pos lexbuf)) }
  | "interface" as s      { mpt "%s\n" s; slt (INTERFACE (get_pos lexbuf)) }
  | "map" as s            { mpt "%s\n" s; slt (MAP (get_pos lexbuf)) }
  | "package" as s        { mpt "%s\n" s; slt (PACKAGE (get_pos lexbuf)) }
  | "range" as s          { mpt "%s\n" s; slt (RANGE (get_pos lexbuf)) }
  | "return" as s         { mpt "%s\n" s; slt (RETURN (get_pos lexbuf)) }
  | "select" as s         { mpt "%s\n" s; slt (SELECT (get_pos lexbuf)) }
  | "struct" as s         { mpt "%s\n" s; slt (STRUCT (get_pos lexbuf)) }
  | "switch" as s         { mpt "%s\n" s; slt (SWITCH (get_pos lexbuf)) }
  | "type" as s           { mpt "%s\n" s; slt (TYPE (get_pos lexbuf)) }
  | "var" as s            { mpt "%s\n" s; slt (VAR (get_pos lexbuf)) }
  | "_"   as c            { mpt "%s\n" "blank_identifier"; slt (IDENT ((String.make 1 c), (get_pos lexbuf))) }
(* 'keyword' functions *)
  | "print" as s          { mpt "%s\n" s; slt (PRINT (get_pos lexbuf)) }
  | "println" as s        { mpt "%s\n" s; slt (PRINTLN (get_pos lexbuf)) }
  | "append" as s         { mpt "%s\n" s; slt (APPEND (get_pos lexbuf)) }
  | "len" as s            { mpt "%s\n" s; slt (LEN (get_pos lexbuf)) }
  | "cap" as s            { mpt "%s\n" s; slt (CAP (get_pos lexbuf)) }
(* punctuation *)
  | "(" as c              { mpt "%c\n" c; slt (LPAREN (get_pos lexbuf)) }
  | ")" as c              { mpt "%c\n" c; slt (RPAREN (get_pos lexbuf)) }
  | "[" as c              { mpt "%c\n" c; slt (LBRACK (get_pos lexbuf)) }
  | "]" as c              { mpt "%c\n" c; slt (RBRACK (get_pos lexbuf)) }
  | "{" as c              { mpt "%c\n" c; slt (LCURLY (get_pos lexbuf)) }
  | "}" as c            { mpt "%c\n" c; slt (RCURLY (get_pos lexbuf)) }
  | ":" as c              { mpt "%c\n" c; slt (COLON (get_pos lexbuf)) }
  | ";" as c              { mpt "%c\n" c; slt (SEMICOLON (get_pos lexbuf)) }
  | "," as c              { mpt "%c\n" c; slt (COMMA (get_pos lexbuf)) }
  | "." as c              { mpt "%c\n" c; slt (DOT (get_pos lexbuf)) }
  | "..." as s            { mpt "%s\n" s; slt (ELLIPSIS (get_pos lexbuf)) }
(* operators *)
  | "+" as c              { mpt "%c\n" c; slt (PLUS (get_pos lexbuf)) } (* arithmetic *)
  | "-" as c              { mpt "%c\n" c; slt (MINUS (get_pos lexbuf)) }
  | "*" as c              { mpt "%c\n" c; slt (MULT (get_pos lexbuf)) }
  | "/" as c              { mpt "%c\n" c; slt (DIV (get_pos lexbuf)) }
  | "%" as c              { mpt "%c\n" c; slt (MOD (get_pos lexbuf)) }
  | "&" as c              { mpt "%c\n" c; slt (BAND (get_pos lexbuf)) } (* bitwise *)
  | "|" as c              { mpt "%c\n" c; slt (BOR (get_pos lexbuf)) }
  | "^" as c              { mpt "%c\n" c; slt (XOR (get_pos lexbuf)) }
  | "<<" as s             { mpt "%s\n" s; slt (LSHFT (get_pos lexbuf)) }
  | ">>" as s             { mpt "%s\n" s; slt (RSHFT (get_pos lexbuf)) }
  | "&^" as s             { mpt "%s\n" s; slt (NAND (get_pos lexbuf)) }
  | "+=" as s             { mpt "%s\n" s; slt (PLUSEQ (get_pos lexbuf)) } (* arithmetic assignment *)
  | "-=" as s             { mpt "%s\n" s; slt (MINUSEQ (get_pos lexbuf)) }
  | "*=" as s             { mpt "%s\n" s; slt (MULTEQ (get_pos lexbuf)) }
  | "/=" as s             { mpt "%s\n" s; slt (DIVEQ (get_pos lexbuf)) }
  | "%=" as s             { mpt "%s\n" s; slt (MODEQ (get_pos lexbuf)) }
  | "++" as s             { mpt "%s\n" s; slt (INC (get_pos lexbuf)) }
  | "--" as s             { mpt "%s\n" s; slt (DEC (get_pos lexbuf)) }
  | "&=" as s             { mpt "%s\n" s; slt (BANDEQ (get_pos lexbuf)) } (* bitwise assignment *)
  | "|=" as s             { mpt "%s\n" s; slt (BOREQ (get_pos lexbuf)) }
  | "^=" as s             { mpt "%s\n" s; slt (XOREQ (get_pos lexbuf)) }
  | "<<=" as s            { mpt "%s\n" s; slt (LSHFTEQ (get_pos lexbuf)) }
  | ">>=" as s            { mpt "%s\n" s; slt (RSHFTEQ (get_pos lexbuf)) }
  | "&^=" as s            { mpt "%s\n" s; slt (NANDEQ (get_pos lexbuf)) }
  | "==" as s             { mpt "%s\n" s; slt (EQ (get_pos lexbuf)) } (* comparison *)
  | "!=" as s             { mpt "%s\n" s; slt (NEQ (get_pos lexbuf)) }
  | "<" as c              { mpt "%c\n" c; slt (LT (get_pos lexbuf)) }
  | "<=" as s             { mpt "%s\n" s; slt (LTEQ (get_pos lexbuf)) }
  | ">" as c              { mpt "%c\n" c; slt (GT (get_pos lexbuf)) }
  | ">=" as s             { mpt "%s\n" s; slt (GTEQ (get_pos lexbuf)) }
  | "!" as c              { mpt "%c\n" c; slt (NOT (get_pos lexbuf)) }
  | "&&" as s             { mpt "%s\n" s; slt (AND (get_pos lexbuf)) }
  | "||" as s             { mpt "%s\n" s; slt (OR (get_pos lexbuf)) }
  | "<-" as s             { mpt "%s\n" s; slt (CHASG (get_pos lexbuf)) } (* assignment *)
  | "=" as c              { mpt "%c\n" c; slt (ASG (get_pos lexbuf)) }
  | ":=" as s             { mpt "%s\n" s; slt (IASG (get_pos lexbuf)) }
(* parametrized *)
  | comment as s          { mpt "comment(%s)\n" s;    scanner lexbuf (* no comments for parser *) }
  | id as s               { mpt "identifier(%s)\n" s; slt (IDENT (s, (get_pos lexbuf))) }
  | lit_int as s          { mpt "lit_int(%s)\n" s;    slt (LIT_INT ((sgi2i s), (get_pos lexbuf))) }
  | lit_float as s        { mpt "lit_float(%s)\n" s;  slt (LIT_FLOAT ((sgf2f s), (get_pos lexbuf))) }
  | lit_bool as s         { mpt "lit_bool(%s)\n" s;   slt (LIT_BOOL ((sgb2b s), (get_pos lexbuf))) }
  | lit_string as s       { mpt "lit_string(%s)\n" s; slt (LIT_STRING (s, (get_pos lexbuf))) }
  | lit_raw_string as s   { mpt "lit_raw_string(%s)\n" s; slt (LIT_RAW_STRING (s, (get_pos lexbuf)))}
  | lit_rune as s         { mpt "lit_rune(%s)\n" s;   slt (LIT_RUNE (s, (get_pos lexbuf))) }
(* special *)
  | [' ' '\t']            { scanner lexbuf }
  | '\n' | '\r'                  { incr_linenum lexbuf;
                            if is_stmt_end !last_token then
                              ( mpt "%s\n" ";" ; slt (SEMICOLON (get_pos lexbuf)) )
                            else scanner lexbuf
                          }
  | eof                   { slt_returns_eof := true;
                            if not (is_semicolon !last_token) then
                              ( mpt "%s\n" ";"; last_token := (SEMICOLON (get_pos lexbuf)); (SEMICOLON (get_pos lexbuf)) )
                            else
                              ( mpt "%s\n" "EOF"; EOF )
                          }
  | _                     { raise Helpers.LexerError }


(*trailer
 * ---------- *)

{
    (* nothing *)
}
