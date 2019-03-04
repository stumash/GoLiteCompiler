open Tree
open Printf

(* pp - Pretty Print *)
let rec pp_prog prog =
    match prog with
    | _ -> p "let's get this shit started!"

(* package *)
and pp_package (Package s) =
    printf "package %s\n" s

(* declaration *)
and pp_decl decl =
    match decl with
    | FunctionDeclaration (id, params, tso, ss) ->
        pp_id id; p "("; pp_params params; p ") "; ifsome tso pp_ts; p " {\n";
        List.iter pp_stmt ss; p "}\n"
    | VariableDeclaration vds ->
        let pp_vd (ids : identifier list)  (tso : type_spec option) (eso : expression list option) =
            pp_idlist ids; p " "; ifsome tso pp_ts; p " "; ifsome eso pp_explist; pln "" in
        (match vds with
        | []   -> p "var ( )\n"
        | [vds] -> p "var "; pp_vd vds
        | vds  -> p "var (\n"; List.iter pp_vd vds; p ")\n")
    | TypeDeclaration tds ->
        let pp_td (id, ts) = pp_id id; p " "; pp_ts ts; pln "" in
        match tds with
        | []   -> p "type ( )\n"
        | [td] -> p "type "; pp_td td
        | tds  -> p "type (\n"; List.iter pp_td tds; p ")\n"

(*parameters printing for functions *)
and pp_params ps : parameters  = 
  let rec id_print ids  = 
    match ids with 
      | h :: [] -> pp_id h
      | h :: t -> pp_id h; p ","; id_print t
      | [] -> pp_id Identifier ("") in
  let final (ids : identifier list) (t: type_spec) = 
    id_print ids; pp_ts t; p "," in
  List.iter (final) ps 
  
(* identifier *)
and pp_id (Identifier s) =
    p s

(*type spec*)
and pp_ts tp = 
    match tp with 
    | IdentifierType id -> pp_id id 
    | ArrayTypeLiteral (e, t) -> p "["; pp_exp; p "]"; pp_ts t
    | StructTypeLiteral str -> pp_param str 
    | SliceTypeLiteral t -> p "[]"; pp_ts t  

(* statement *)
and pp_stmt stmt =
    match stmt with
    | ExpressionStatement e               -> pp_exp e; pln ""
    | AssignmentStatement (es1, aop, es2) -> pp_explist es1; pp_aop aop; pp_explist es2; pln ""
    | DeclarationStatement decl           -> pp_decl decl
    | ShortValDeclaration (ids, es)       -> pp_idlist ids; p " := "; pp_explist es; pln ""
    | Inc e                               -> pp_exp e; p "++"; pln ""
    | Dec e                               -> pp_exp e; p "--"; pln ""
    | PrintStatement es                   -> p "print("; pp_explist es; pln ")"
    | PrintlnStatement es                 -> p "println("; pp_explist es; pln ")"
    | ReturnStatement eo                  -> p "return "; ifsome eo pp_exp; pln ""
    | IfStatement ifs                     -> pp_ifs ifs
    | SwitchStatement (so, eo, scs)       -> p "switch"; pp_stmt so; p ";"; pp_eo; p ";"; pln "{"; pp_sc scs; pln "}"
    | ForStatement (so, eo, so, ss)       -> pp_for fs
    | Break                               -> pln "break"
    | Continue                            -> pln "continue"
    | _                                   -> p "" (*For statement option when it is None in if statement*)


(* assignment operator *)
and pp_aop aop =
    match aop with
    | ASG -> p " = "
    | PLUSEQ -> p " += "
    | MINUSEQ -> p " -= "
    | MULTEQ -> p " *= "
    | DIVEQ -> p " /= "
    | MODEQ -> p " %= "
    | BANDEQ -> p " &= "
    | BOREQ -> p " |= "
    | XOREQ -> p " ^= "
    | LSHFTEQ -> p " <<= "
    | RSHFTEQ -> p " >>= "
    | NANDEQ -> p " &^= "

(* if statement *)
and pp_ifs (If (so, e, ss, elso)) =
    p "if "; pp_stmt so; p ";"; pp_exp e
    pln "{"
    List.iter (pp_stmt) ss
    pln "}"
    pp_else elso

(* else statement *)
and pp_els els = 
    match els with
    | Elseif ifs -> p "else "; pln "{"; pp_ifs ifs; pln "}"  
    | Else ss -> p "else"; pln "{"; List.iter (pp_stmt) ss; pln "}"

(* switch clause *)
and pp_sc sc =
    match sc with
    | Default ss -> p "default :"; List.iter (pp_stmt) ss; pln ""
    | Case (es, ss) -> p "case "; List.iter (pp_exp) es; p ":"; List.iter (pp_stmt) ss; pln "" 

(*for statements *)
and pp_for (ForStatement(so, eo, so, sl)) = 
  p "for"; pp_stmt so; p ";"; pp_exp eo; p ";"; pp_stmt so
  pln "{"
  List.iter (pp_stmt) sl
  pln "}"


    

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
    | FunctionCall (s, es)         -> printf "%s(" s; pp_explist es; p ")"
    | Append (e1, e2)              -> p "append("; pp_explist [e1;e2]; p ")"
    | Len (e)                      -> p "len("; pp_exp e; p ")"
    | Cap (e)                      -> p "cap("; pp_exp e; p ")"
    | ParenExpression (e)          -> p "("; pp_exp e; p ")"
    | LitInt (i)                   -> print_int i
    | LitFloat (f)                 -> print_float f
    | LitBool (b)                  -> printf "%B" b; ()
    | LitRune (s)                  -> p s
    | LitString (s)                -> p s
    | IdentifierExpression (idexp) -> pp_idexp idexp

and pp_idexp idexp =
    match idexp with
    | Ident (s)                -> p s
    | Blankid                  -> p "_"
    | Indexed (s, e)           -> printf "%s[" s; pp_exp e; p "]"
    | StructAccess (s, idexp)  -> printf "%s." s; pp_idexp idexp

(* helpers *)

and p = print_string
and pln = print_endline
and pp_explist es =
    let f = (fun i e -> pp_exp e; if i != (List.length es)-1 then p ", " else ()) in
    List.iteri f es
and pp_idlist ids =
    let f = (fun i id -> pp_id id; if i != (List.length ids)-1 then p ", " else ()) in
    List.iteri f ids

and ifsome o f =
    match o with
    | Some a -> f a
    | _ -> ()
