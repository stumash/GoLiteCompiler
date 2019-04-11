open Cactus_stack
open Golitetypes
open Tree
open Helpers

let () =
    let lb = Lexing.from_channel stdin in
    try
        let ast = Parse.program Lexer.scanner lb in
        Typecheck.type_check_prog ast;
        Codegen.cg_program ast;
        print_endline "#OK: Codegen"
    with
    | Parse.Error -> Helpers.print_error lb "Parser"
    | e           -> Helpers.handle_error lb e ~default:"TypeCheckerDefault"

