(* ERROR HELPERS *)

(* print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let {pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
    let cnum = pos_cnum - pos_bol - ((lexeme_end lexbuf) - (lexeme_start lexbuf)) in
    Printf.eprintf "Error: '%s' at L%d,C%d: '%s'\n" (lexeme lexbuf) pos_lnum cnum compiler_stage);
    exit 1; ()

exception LexerError
exception ExpressionIsNotIdentifier
exception VarDecNeedsTypeOrInit
exception VarDecIdsLenNeqExpsLen
exception NotSimpleStatement
exception MultAsgCannotShorthand
exception SwitchMultipleDefaults
exception TypeCheckError of string
exception TypeCheckParserError of string

let handle_error ?(default="Default") lb = function
    | TypeCheckError str        -> print_error lb @@ "TypeChecker: " ^ str
    | TypeCheckParserError str  -> print_error lb @@ "TypeChecker: ParserError: " ^ str
    | ExpressionIsNotIdentifier -> print_error lb "Parser: exp is not ident"
    | VarDecIdsLenNeqExpsLen    -> print_error lb "Parser: var. decl. LHS size unequal RHS size"
    | VarDecNeedsTypeOrInit     -> print_error lb "Parser: var. decl. needs type or initializer"
    | NotSimpleStatement        -> print_error lb "Parser: statement is not a 'simple statement'"
    | MultAsgCannotShorthand    -> print_error lb "Parser: multiple assignment must use '='"
    | SwitchMultipleDefaults    -> print_error lb "Parser: switch has multiple default cases"
    | LexerError                -> print_error lb "Scanner"
    | _                         -> print_error lb default


(* GENERAL HELPERS *)

let ifsome o f =
    match o with
    | None -> ()
    | Some a -> f a

let err_if b err =
    if b then raise err else ()
