open Tree
open Helpers
open Printf

(* local helpers *)

(* pretty print a list of xs, comma-separated, using a given pretty printer for type x *)
let pp_comma_separated_xs xs pp_x =
    let f i x = pp_x x; if i != (List.length xs)-1 then print_string ", " else () in
    List.iteri f xs

(*-----------------------------------*
 * pp - Pretty Print
 *-----------------------------------*)

(* program *)
let rec pp_prog prog =
    match prog with
    | Program (pkg, ds) -> pp_pkg pkg; List.iter pp_decl ds
    | EmptyProgram      -> p ""

(* package *)
and pp_pkg (Package str) =
    printf "package %s\n" str

(* declaration *)
and pp_decl decl =
    match decl with
    | FunctionDeclaration (id, prms, tpo, ss) ->
        pp_id id; p "("; pp_prms prms; p ") "; ifsome tpo pp_tp; p " {\n";
        List.iter pp_stmt ss;
        p "}\n"
    | VariableDeclaration vds ->
        let pp_vd (ids, tpo, eso) =
            pp_idlist ids; p " "; ifsome tpo pp_tp;
            ifsome eso (fun es -> p " = "; pp_explist es); p "\n" in
        (match vds with
        | []   -> () (* remove empty variable declarations *)
        | [vd] -> p "var "; pp_vd vd
        | vds  -> p "var (\n"; List.iter pp_vd vds; p ")\n")
    | TypeDeclaration tds ->
        let pp_td (id, tp) = pp_id id; p " "; pp_tp tp; p "\n" in
        (match tds with
        | []   -> () (* remove empty type declarations *)
        | [td] -> p "type "; pp_td td
        | tds  -> p "type (\n"; List.iter pp_td tds; p ")\n")

(* identifier *)
and pp_id (Identifier str) =
    p str

(* type_spec *)
and pp_tp tp =
    match tp with
    | IdentifierType (Identifier str) -> p str
    | ArrayTypeLiteral (e, tp)        -> p "["; pp_exp e; p "]"; pp_tp tp
    | StructTypeLiteral prms          -> p "{\n"; List.iter (fun prm -> pp_prm prm; p "\n") prms; p "}\n"
    | SliceTypeLiteral tp             -> p "[]"; pp_tp tp

(* function paramters *)
and pp_prms (Parameters prms) =
    pp_comma_separated_xs prms pp_prm

and pp_prm (ids, tp) =
    pp_idlist ids; p " "; pp_tp tp

(* statement *)
(* bool nl: if true print trailing newline in 'simple statementp' *)
and pp_stmt ?(nl=true) stmt =
    match stmt with
    (* simple statements *)
    | ExpressionStatement e               -> pp_exp e; ifp nl "\n"
    | AssignmentStatement (es1, aop, es2) -> pp_explist es1; pp_aop aop; pp_explist es2; ifp nl "\n"
    | ShortValDeclaration (ids, es)       -> pp_idlist ids; p " := "; pp_explist es; ifp nl "\n"
    | Inc e                               -> pp_exp e; p "++"; ifp nl "\n"
    | Dec e                               -> pp_exp e; p "--"; ifp nl "\n"
    | EmptyStatement                      -> ifp nl "\n"
    (* all other statements *)
    | DeclarationStatement decl           -> pp_decl decl
    | PrintStatement Some es              -> p "print("; pp_explist es; p ")\n"
    | PrintlnStatement Some es            -> p "println("; pp_explist es; p ")\n"
    | PrintStatement None                   -> p "print ()\n"
    | PrintlnStatement None                -> p "println ()\n"
    | ReturnStatement eo                  -> p "return "; ifsome eo pp_exp; p "\n"
    | IfStatement ifs                     -> pp_ifs ifs
    | SwitchStatement (so, eo, scl)       -> pp_sw (so, eo, scl)
    | ForStatement (so1, eo, so2, ss)     -> pp_for (so1, eo, so2, ss)
    | Break                               -> p "break\n"
    | Continue                            -> p "continue\n"
    | BlockStatements ss                  -> p "{\n"; List.iter pp_stmt ss; p "}\n"

(* assignment operator *)
and pp_aop aop =
    match aop with
    | ASG     -> p " = "
    | PLUSEQ  -> p " += "
    | MINUSEQ -> p " -= "
    | MULTEQ  -> p " *= "
    | DIVEQ   -> p " /= "
    | MODEQ   -> p " %= "
    | BANDEQ  -> p " &= "
    | BOREQ   -> p " |= "
    | XOREQ   -> p " ^= "
    | LSHFTEQ -> p " <<= "
    | RSHFTEQ -> p " >>= "
    | NANDEQ  -> p " &^= "

(* if statement *)
and pp_ifs (If (s, e, ss, elso)) =
    p "if "; pp_stmt ~nl:false s; p "; "; pp_exp e; p " {\n";
    List.iter pp_stmt ss;
    p "}";
    match elso with
    | None     -> p "\n"
    | Some els -> p " "; pp_els els

(* else statement *)
and pp_els els =
    match els with
    | Elseif ifs -> p "else "; pp_ifs ifs
    | Else ss    -> p "else {\n"; List.iter pp_stmt ss; p "}\n"

(* switch statemtent *)
and pp_sw (s, eo, scs) =
    p "switch "; pp_stmt ~nl:false s; p ";"; ifsome eo pp_exp; p " {\n";
    List.iter pp_sc scs;
    p "}\n"

(* switch clause *)
and pp_sc sc =
    (* TODO *)
    match sc with
    | Default ss    -> p "default {\n"; List.iter pp_stmt ss; p "}\n"
    | Case (es, ss) -> p "case "; pp_explist es; p " {\n"; List.iter pp_stmt ss; p "}\n"

and pp_for (s1, eo, s2, ss) =
    p "for ";
    pp_stmt ~nl:false s1; p "; "; ifsome eo pp_exp; p "; "; pp_stmt ~nl:false s2;
    p " {\n";
    List.iter pp_stmt ss;
    p "}\n"

(* expression *)
and pp_exp exp =
    match exp with
    | Or (e1, e2)                  -> pp_exp e1; p " || "; pp_exp e2
    | And (e1, e2)                 -> pp_exp e1; p " && "; pp_exp e2
    | Eq (e1, e2)                  -> pp_exp e1; p " == "; pp_exp e2
    | Neq (e1, e2)                 -> pp_exp e1; p " != "; pp_exp e2
    | Gt (e1, e2)                  -> pp_exp e1; p " > "; pp_exp e2
    | Gteq (e1, e2)                -> pp_exp e1; p " >= "; pp_exp e2
    | Lt (e1, e2)                  -> pp_exp e1; p " < "; pp_exp e2
    | Lteq (e1, e2)                -> pp_exp e1; p " <= "; pp_exp e2
    | Plus (e1, e2)                -> pp_exp e1; p " + "; pp_exp e2
    | Minus (e1, e2)               -> pp_exp e1; p " - "; pp_exp e2
    | Bor (e1, e2)                 -> pp_exp e1; p " | "; pp_exp e2
    | Xor (e1, e2)                 -> pp_exp e1; p " ^ "; pp_exp e2
    | Mult (e1, e2)                -> pp_exp e1; p " * "; pp_exp e2
    | Div (e1, e2)                 -> pp_exp e1; p " / "; pp_exp e2
    | Mod (e1, e2)                 -> pp_exp e1; p " % "; pp_exp e2
    | Lshft (e1, e2)               -> pp_exp e1; p " << "; pp_exp e2
    | Rshft (e1, e2)               -> pp_exp e1; p " >> "; pp_exp e2
    | Band (e1, e2)                -> pp_exp e1; p " & "; pp_exp e2
    | Nand (e1, e2)                -> pp_exp e1; p " &^ "; pp_exp e2
    | Uplus (e)                    -> p " + "; pp_exp e
    | Uminus (e)                   -> p " - "; pp_exp e
    | Not (e)                      -> p " ! "; pp_exp e
    | Uxor (e)                     -> p " ^ "; pp_exp e
    | FunctionCall (str, es)       -> printf "%s(" str; pp_explist es; p ")"
    | Append (e1, e2)              -> p "append("; pp_explist [e1;e2]; p ")"
    | Len (e)                      -> p "len("; pp_exp e; p ")"
    | Cap (e)                      -> p "cap("; pp_exp e; p ")"
    | ParenExpression (e)          -> p "("; pp_exp e; p ")"
    | LitInt (i)                   -> print_int i
    | LitFloat (f)                 -> print_float f
    | LitBool (b)                  -> printf "%B" b; ()
    | LitRune (str)                -> p str
    | LitString (str)              -> p str
    | LitRawString (str)           -> p str
    | IdentifierExpression (idexp) -> pp_idexp idexp

and pp_idexp idexp =
    match idexp with
    | Ident (str)               -> p str
    | Indexed (str, e)          -> printf "%s[" str; pp_exp e; p "]"
    | StructAccess (str, idexp) -> printf "%s." str; pp_idexp idexp

(* helpers *)

and p str = print_string str
and ifp b str = if b then p str else ()

and pp_explist es = pp_comma_separated_xs es pp_exp
and pp_idlist ids = pp_comma_separated_xs ids pp_id
