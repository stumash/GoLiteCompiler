open Tree
open Helpers
open Printf

(* local helpers *)

(* pretty print a list of xs, comma-separated, using a given pretty printer for type x *)
let pp_comma_separated_xs xs pp_x =
    let f i x = pp_x x; if i != (List.length xs)-1 then print_string ", " else () in
    List.iteri f xs

(* print an indentation given an indent level *)
let p_ind il =
    print_string @@ String.make (il * 2) ' '

(*-----------------------------------*
 * pp - Pretty Print
 *-----------------------------------*)

(* program *)
let rec pp_prog prog =
    match prog with
    | Program (pkg, ds) -> pp_pkg pkg; List.iter pp_decl ds
    | EmptyProgram      -> p ""

(* package *)
and pp_pkg (Package (str,_)) =
    printf "package %s\n" str

(* declaration *)
and pp_decl ?(il=0) decl =
    match decl with
    | FunctionDeclaration (id, prms, tpo, ss, _) ->
        p_ind il; pp_id id; p "("; pp_prms prms; p ") "; ifsome tpo pp_tp; p " {\n";
        List.iter pp_stmt ss;
        p "}\n"
    | VariableDeclaration (vds,_) ->
        let pp_vd (ids, tpo, eso) =
            pp_ids ids; p " "; ifsome tpo pp_tp;
            ifsome eso (fun es -> p " = "; pp_exps es); p "\n" in
        (match vds with
        | []   -> () (* remove empty variable declarations *)
        | [vd] -> p_ind il; p "var "; pp_vd vd
        | vds  -> p_ind il; p "var (\n"; List.iter pp_vd vds; p ")\n")
    | TypeDeclaration (tds,_) ->
        let pp_td (id, tp) = pp_id id; p " "; pp_tp tp; p "\n" in
        (match tds with
        | []   -> () (* remove empty type declarations *)
        | [td] -> p_ind il; p "type "; pp_td td
        | tds  -> p_ind il; p "type (\n"; List.iter pp_td tds; p ")\n")

(* identifier *)
and pp_id (Identifier (str,_)) =
    p str

(* type_spec *)
and pp_tp tp =
    match tp with
    | IdentifierType id           -> pp_id id
    | ArrayTypeLiteral (e, tp, _) -> p "["; pp_exp e; p "]"; pp_tp tp
    | StructTypeLiteral (stds,_)  ->
        p "struct{\n";
        List.iter (fun (id,tp) -> pp_id id; p " "; pp_tp tp; p "\n") stds;
        p "}\n"
    | SliceTypeLiteral (tp,_)     -> p "[]"; pp_tp tp

(* function paramters *)
and pp_prms (Parameters prms) =
    pp_comma_separated_xs prms pp_prm

and pp_prm (ids, tp) =
    pp_ids ids; p " "; pp_tp tp

(* statement *)
(* bool nl: if true print trailing newline in 'simple statementp' *)
(* int il: indent level *)
and pp_stmt ?(il=0) ?(nl=true) stmt =
    match stmt with
    (* simple statements *)
    | ExpressionStatement (e,_)           -> p_ind il; pp_exp e; ifp nl "\n"
    | AssignmentStatement (es1, aop, es2) -> p_ind il; pp_exps es1; pp_aop aop; pp_exps es2; ifp nl "\n"
    | ShortValDeclaration (ids, es)       -> p_ind il; pp_ids ids; p " := "; pp_exps es; ifp nl "\n"
    | Inc e                               -> p_ind il; pp_exp e; p "++"; ifp nl "\n"
    | Dec e                               -> p_ind il; pp_exp e; p "--"; ifp nl "\n"
    | EmptyStatement                      -> ifp nl "\n"
    (* all other statements *)
    | DeclarationStatement decl           -> pp_decl ~il decl
    | PrintStatement (Some es,_)          -> p_ind il; p "print("; pp_exps es; p ")\n"
    | PrintlnStatement (Some es,_)        -> p_ind il; p "println("; pp_exps es; p ")\n"
    | PrintStatement (None,_)             -> p_ind il; p "print ()\n"
    | PrintlnStatement (None,_)           -> p_ind il; p "println ()\n"
    | ReturnStatement (eo,_)              -> p_ind il; p "return "; ifsome eo pp_exp; p "\n"
    | IfStatement ifs                     -> pp_ifs ifs
    | SwitchStatement (so, eo, scl, _)    -> pp_sw (so, eo, scl)
    | ForStatement (so1, eo, so2, ss, _)  -> pp_for (so1, eo, so2, ss)
    | Break _                             -> p_ind il; p "break\n"
    | Continue _                          -> p_ind il; p "continue\n"
    | BlockStatements (ss,_)              -> p_ind il; p "{\n"; List.iter (pp_stmt ~il:(il+1)) ss; p_ind il; p "}\n"

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
and pp_ifs (If (s, e, ss, elso, _)) =
    p "if "; pp_stmt ~nl:false s; p "; "; pp_exp e; p " {\n";
    List.iter pp_stmt ss;
    p "}";
    match elso with
    | None     -> p "\n"
    | Some els -> p " "; pp_els els

(* else statement *)
and pp_els els =
    match els with
    | Elseif (ifs,_) -> p "else "; pp_ifs ifs
    | Else (ss,_)    -> p "else {\n"; List.iter pp_stmt ss; p "}\n"

(* switch statemtent *)
and pp_sw (s, eo, scs) =
    p "switch "; pp_stmt ~nl:false s; p ";"; ifsome eo pp_exp; p " {\n";
    List.iter pp_sc scs;
    p "}\n"

(* switch clause *)
and pp_sc sc =
    (* TODO *)
    match sc with
    | Default (ss,_)   -> p "default {\n"; List.iter pp_stmt ss; p "}\n"
    | Case (es, ss, _) -> p "case "; pp_exps es; p " {\n"; List.iter pp_stmt ss; p "}\n"

and pp_for (s1, eo, s2, ss) =
    p "for ";
    pp_stmt ~nl:false s1; p "; "; ifsome eo pp_exp; p "; "; pp_stmt ~nl:false s2;
    p " {\n";
    List.iter pp_stmt ss;
    p "}\n"

(* expression *)
and pp_exp exp =
    match exp with
    (* binary expression *)
    | Or (e1, e2, _)             -> pp_exp e1; p " || "; pp_exp e2
    | And (e1, e2, _)            -> pp_exp e1; p " && "; pp_exp e2
    | Eq (e1, e2, _)             -> pp_exp e1; p " == "; pp_exp e2
    | Neq (e1, e2, _)            -> pp_exp e1; p " != "; pp_exp e2
    | Gt (e1, e2, _)             -> pp_exp e1; p " > "; pp_exp e2
    | Gteq (e1, e2, _)           -> pp_exp e1; p " >= "; pp_exp e2
    | Lt (e1, e2, _)             -> pp_exp e1; p " < "; pp_exp e2
    | Lteq (e1, e2, _)           -> pp_exp e1; p " <= "; pp_exp e2
    | Plus (e1, e2, _)           -> pp_exp e1; p " + "; pp_exp e2
    | Minus (e1, e2, _)          -> pp_exp e1; p " - "; pp_exp e2
    | Bor (e1, e2, _)            -> pp_exp e1; p " | "; pp_exp e2
    | Xor (e1, e2, _)            -> pp_exp e1; p " ^ "; pp_exp e2
    | Mult (e1, e2, _)           -> pp_exp e1; p " * "; pp_exp e2
    | Div (e1, e2, _)            -> pp_exp e1; p " / "; pp_exp e2
    | Mod (e1, e2, _)            -> pp_exp e1; p " % "; pp_exp e2
    | Lshft (e1, e2, _)          -> pp_exp e1; p " << "; pp_exp e2
    | Rshft (e1, e2, _)          -> pp_exp e1; p " >> "; pp_exp e2
    | Band (e1, e2, _)           -> pp_exp e1; p " & "; pp_exp e2
    | Nand (e1, e2, _)           -> pp_exp e1; p " &^ "; pp_exp e2
    (* unary *)
    | Uplus (e, _)               -> p " + "; pp_exp e
    | Uminus (e, _)              -> p " - "; pp_exp e
    | Not (e, _)                 -> p " ! "; pp_exp e
    | Uxor (e, _)                -> p " ^ "; pp_exp e
    (* function calls *)
    | FunctionCall (id, es, _)   -> pp_id id; p "("; pp_exps es; p ")"
    | Append (e1, e2, _)         -> p "append("; pp_exps [e1;e2]; p ")"
    | Len (e, _)                 -> p "len("; pp_exp e; p ")"
    | Cap (e, _)                 -> p "cap("; pp_exp e; p ")"
    | ParenExpression (e, _)     -> p "("; pp_exp e; p ")"
    (* misc *)
    | LitInt (i, _)              -> print_int i
    | LitFloat (f, _)            -> print_float f
    | LitBool (b, _)             -> printf "%B" b; ()
    | LitRune (str, _)           -> p str
    | LitString (str, _)         -> p str
    | LitRawString (str, _)      -> p str
    | IdentifierExpression idexp -> pp_idexp idexp

and pp_idexp idexp =
    match idexp with
    | Ident (str, _)             -> p str
    | Indexed (exp, e, _)        -> pp_exp exp; p "["; pp_exp e; p "]"
    | StructAccess (exp, str, _) -> pp_exp exp; printf ".%s" str

(* helpers *)

and p str = print_string str
and ifp b str = if b then p str else ()

and pp_exps es = pp_comma_separated_xs es pp_exp
and pp_ids ids = pp_comma_separated_xs ids pp_id
