(* ERROR HELPERS *)

let get_pos lexbuf = Lexing.(
    let {pos_lnum; pos_cnum; pos_bol} = lexbuf.lex_curr_p in
    let colnum = pos_cnum - pos_bol - ((lexeme_end lexbuf) - (lexeme_start lexbuf)) in
    (pos_lnum, colnum))

(* print the compiler stage that detected the error, and its cause *)
let print_error lexbuf compiler_stage = Lexing.(
    let pos_lnum, pos_cnum = get_pos lexbuf in
    Printf.eprintf "Error: '%s'  '%s'\n" (lexeme lexbuf) compiler_stage);
    exit 1; ()

exception LexerError
exception ExpressionIsNotIdentifier
exception VarDecNeedsTypeOrInit
exception VarDecIdsLenNeqExpsLen
exception NotSimpleStatement
exception MultAsgCannotShorthand
exception SwitchMultipleDefaults
exception TypeCheckError of string * (int * int)
exception TypeCheckParserError of string * (int * int)
exception BreakContinueError of string * (int * int) 

let z (lineno,colno) = " at L" ^ (string_of_int lineno) ^ ",C" ^ (string_of_int colno)

let handle_error ?(default="Default") lb = function
    | TypeCheckError (str, pos)       -> print_error lb @@ "TypeChecker: " ^ str ^ (z pos)
    | TypeCheckParserError (str, pos) -> print_error lb @@ "TypeChecker: ParserError: " ^ str ^ (z pos)
    | ExpressionIsNotIdentifier       -> print_error lb "Parser: exp is not ident"
    | VarDecIdsLenNeqExpsLen          -> print_error lb "Parser: var. decl. LHS size unequal RHS size"
    | VarDecNeedsTypeOrInit           -> print_error lb "Parser: var. decl. needs type or initializer"
    | NotSimpleStatement              -> print_error lb "Parser: statement is not a 'simple statement'"
    | MultAsgCannotShorthand          -> print_error lb "Parser: multiple assignment must use '='"
    | SwitchMultipleDefaults          -> print_error lb "Parser: switch has multiple default cases"
    | BreakContinueError (str,pos)    -> print_error lb @@ "Parser: " ^ str ^ (z pos)
    | LexerError                      -> print_error lb "Scanner"
    | _                               -> print_error lb default


(* GENERAL HELPERS *)

let ifsome o f =
    match o with
    | None -> ()
    | Some a -> f a

let err_if b err =
    if b then raise err else ()

let dup_exists xs =
  let rec f acc xs =
      match xs with
      | h1::(h2::_ as t) -> if h1 = h2 then true else f acc t
      | _ -> acc in
  f false (List.sort compare xs)

let find_dup xs =
  let rec f xs =
      match xs with
      | h1::(h2::_ as t) -> if h1 = h2 then h2 else f t
      | _ -> raise (Failure "IMPOSSIBLE: no dups found") in
  f (List.sort compare xs)

let fst3 (one, two, three) = one
let snd3 (one, two, three) = two
let trd3 (one, two, three) = three
