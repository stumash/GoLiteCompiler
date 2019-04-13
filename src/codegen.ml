open Tree
open Typecheck
open Helpers
open Cactus_stack
open Golitetypes

(* append positions to variable names for overshadowing *)
(* for every non-declared variable in current scope, use 'nonlocal' on it *)
(* copy on EVERY RHS and ALL FUNC ARGS *)

let main_function_exists = ref false

let current_scope = ref global_scope

let enter_first_child_scope () =
    current_scope := List.hd (List.rev !(children !current_scope))

let enter_parent_and_delete_first_child_scope () =
    let cs_node_remove = !current_scope in
    current_scope := parent !current_scope;
    let f node = node != cs_node_remove in
    (children !current_scope) := List.filter f !(children !current_scope)

let p s = print_string s

let p_ind il = p (String.make (il * 4) ' ')

let z (ln,cn) =
    if (ln,cn) = (-2,-2) then "" else
    let f i = if i < 0 then "_"^(string_of_int (-1 * i)) else string_of_int i in
    "_"^(f ln)^"_"^(f cn)

let get_str_from_idexp idexp =
    let rec get_str_from_idexp' idexp =
        (match idexp with
        | Ident (str, pos) -> str, pos
        | Indexed (IdentifierExpression e, _, _) -> get_str_from_idexp'  e
        | StructAccess (IdentifierExpression e, _, _) -> get_str_from_idexp' e
        | _ -> raise (CodegenError "WHY YOU DO THIS TO ME ? ")) in
    get_str_from_idexp' idexp

let dec_lookup str pos = (* get position of declaration for Identifier(str, pos) *)
    let cat,glt,dec_pos = lookup ~current_scope str pos in
    (* TODO: remove this: print_endline (z dec_pos);*)
    if dec_pos <= pos then cat,glt,dec_pos else
    lookup ~current_scope:(ref (parent !current_scope)) str pos

let get_and_print_nonlocals ?(il=0) ss scope_start =
    let non_local_strs =
        let f acc s =
            let s_nonlocals =
                match s with
                | AssignmentStatement (es1, ao, es2) ->
                    let f acc e =
                        (match e with
                        | IdentifierExpression idexp ->
                            let str, pos = get_str_from_idexp idexp in
                            let dec_pos = trd3 (dec_lookup str pos) in
                            if dec_pos < scope_start then (str^(z dec_pos)) :: acc else acc
                        | _ -> acc) in
                    List.fold_left f [] es1
                | Inc (IdentifierExpression idexp) | Dec (IdentifierExpression idexp) ->
                    let str, pos = get_str_from_idexp idexp in
                    let dec_pos = trd3 (dec_lookup str pos) in
                    if dec_pos < scope_start then [str^(z dec_pos)] else []
                | _ -> [] in
            s_nonlocals :: acc in
        remove_dups (List.flatten (List.fold_left f [] ss)) in
    let f str = (p_ind il; p ("nonlocal "^str^"\n")) in
    List.iter f non_local_strs

let rec cg_glt_default ?(il=0) varname glt =
    match glt with
    | Void | FunctionT (_,_) -> raise (CodegenError "WHY YOU DO THIS TO ME?!")
    | IntT                   -> p_ind il; p (varname^" = 0\n")
    | FloatT                 -> p_ind il; p (varname^" = 0.0\n")
    | BoolT                  -> p_ind il; p (varname^" = False\n")
    | RuneT                  -> p_ind il; p (varname^" = 0\n")
    | StringT                -> p_ind il; p (varname^" = \"\"\n")
    | SliceT _               -> p_ind il; p (varname^" = go_slice()\n")
    | NamedT (str, pos)      -> cg_glt_default ~il varname (rt ~current_scope glt)
    | ArrayT (i,glt)         ->
        p_ind il; p "def make_array_element():\n";
        cg_glt_default ~il:(il+1) "dummy_var" glt;
        p_ind (il+1); p "return dummy_var\n";
        p_ind il; p (varname^" = [make_array_element() for _ in range("^(string_of_int i)^")]\n")
    | StructT stds           ->
        p_ind il; p (varname^" = go_struct()\n");
        let f (f_name,glt) =
            cg_glt_default ~il (varname^"."^f_name) glt in
        List.iter f stds

let rec cg_program ast =
    match ast with
    | Program (pkg, ds) ->
        let ic = open_in "src/imports.py" in
        let python3_header = really_input_string ic (in_channel_length ic) in
        flush stdout;
        close_in ic;
        p python3_header;
        p "def main():\n";
        (if (List.length ds) = 0 then p "    pass" else ());
        List.iter (cg_decl ~il:1) ds;
        if not !main_function_exists then ()
        else
            (let pos = trd3 (lookup ~current_scope "main" (-1,-1)) in
            p_ind 1; p ("main"^(z pos)^"()\n"));
        p "main()\n\n"
    | EmptyProgram -> ()

and cg_decl ?(il=0) dec =
    match dec with
    | FunctionDeclaration (Identifier (str, pos1) as id, para, tso, ss, pos2) ->
        if str = "main" then main_function_exists := true else ();

        p "\n"; p_ind il;
        p "def "; p (str^z(pos1)); p "(";

        enter_first_child_scope ();

        (match para with
        | Parameters prms ->
            (let f1 (paras, tp) =
                (let f2 id   =
                    cg_id id; p "," in
                List.iter f2 paras) in
            List.iter f1 prms));
        p "):\n";

        get_and_print_nonlocals ~il:(il+1) ss pos2;

        List.iter (cg_stmt ~il:(il+1)) ss;
        if (List.length ss) = 0 then (p_ind (il+1); p "pass\n") else ();
        if str = "init" then (p_ind il; p ("init"^(z pos1)^"()\n")) else ();
        p "\n";

        enter_parent_and_delete_first_child_scope ()
    | VariableDeclaration (vds, pos) ->
        let cg_vd (ids, tso, eso) =
            let eos =
                (match eso with
                | Some es -> List.map (fun e -> Some e) es
                | None    -> List.init (List.length ids) (fun i -> None)) in
            let glts_pos =
                let f (Identifier (str, pos)) =
                    if str = "_" then NamedT("int",(-1,-1)),pos else
                    let (_, glt, pos) = dec_lookup  str pos in
                    glt,pos in
                List.map f ids in
            let f ((Identifier (str,_) as id), ((glt,pos), eo)) =
                (match eo with
                | Some e -> cg_id ~il id; p " = "; p "cpy("; cg_exp e; p ")"
                | None   -> cg_glt_default ~il (str^(z pos)) glt);
                p "\n" in
            List.iter f (List.combine ids (List.combine glts_pos eos)) in
        List.iter cg_vd vds
    | TypeDeclaration (tds, _) -> ()

and cg_id ?(il=0) (Identifier (str, pos)) =
    p_ind il;
    if str = "_" then p (str^(z pos)) else
    let dec_pos = trd3 (dec_lookup str pos) in
    p (str^(z dec_pos))

and cg_exp ?(il=0) e =
    match e with
    (* binary expressions *)
    | Or (e1, e2, _)               -> p_ind il; cg_exp e1; p " or "; cg_exp e2
    | And (e1, e2, _)              -> p_ind il; cg_exp e1; p " and "; cg_exp e2
    | Eq (e1, e2, _)               -> p_ind il; cg_exp e1; p " == "; cg_exp e2
    | Neq (e1, e2, _)              -> p_ind il; cg_exp e1; p " != "; cg_exp e2
    | Gt (e1, e2, _)               -> p_ind il; cg_exp e1; p " > "; cg_exp e2
    | Gteq (e1, e2, _)             -> p_ind il; cg_exp e1; p " >= "; cg_exp e2
    | Lt (e1, e2, _)               -> p_ind il; cg_exp e1; p " < "; cg_exp e2
    | Lteq (e1, e2, _)             -> p_ind il; cg_exp e1; p " <= "; cg_exp e2
    | Plus (e1, e2, _)             -> p_ind il; cg_exp e1; p " + "; cg_exp e2
    | Minus (e1, e2, _)            -> p_ind il; cg_exp e1; p " - "; cg_exp e2
    | Bor (e1, e2, _)              -> p_ind il; cg_exp e1; p " | "; cg_exp e2
    | Xor (e1, e2, _)              -> p_ind il; cg_exp e1; p " ^ "; cg_exp e2
    | Mult (e1, e2, _)             -> p_ind il; cg_exp e1; p " * "; cg_exp e2
    | Div (e1, e2, _)              -> p_ind il; cg_exp e1; p " // "; cg_exp e2
    | Mod (e1, e2, _)              -> p_ind il; cg_exp e1; p " % "; cg_exp e2
    | Lshft (e1, e2, _)            -> p_ind il; cg_exp e1; p " << "; cg_exp e2
    | Rshft (e1, e2, _)            -> p_ind il; cg_exp e1; p " >> "; cg_exp e2
    | Band (e1, e2, _)             -> p_ind il; cg_exp e1; p " & "; cg_exp e2
    | Nand (e1, e2, _)             -> p_ind il; p "~("; cg_exp e1; p " & "; cg_exp e2; p ")"
    (* unary expressions *)
    | Uplus (e, _)                 -> p_ind il; p " + "; cg_exp e
    | Uminus (e, _)                -> p_ind il; p " - "; cg_exp e
    | Not (e, _)                   -> p_ind il; p " not "; cg_exp e
    | Uxor (e, _)                  -> p_ind il; p " ~ "; cg_exp e
    (* misc *)
    | LitInt (i, _)                -> p_ind il; p (string_of_int i)
    | LitBool (b, _)               -> p_ind il; if b == true then p "True" else p "False"
    | LitFloat (f, _)              -> p_ind il; p (string_of_float f)
    | LitString (str, _)           -> p_ind il; p str
    | LitRawString (str, _)        -> p_ind il; p str
    | LitRune (r, _)               -> p_ind il; p "ord ("; p r; p ")"
    | ParenExpression (e, _)       -> p_ind il; p "("; cg_exp e; p ")"
    | IdentifierExpression ie ->
        (match ie with
        | Ident (str, pos)           -> cg_id (Identifier (str, pos))
        | Indexed (e1, e2, pos)      -> cg_exp e1; p "["; cg_exp e2; p "]"
        | StructAccess (e, str, pos) -> cg_exp e; p ("."^str))
    (* function calls *)
    | Append (e1, e2, _)           -> p_ind il; p "append("; cg_exp e1; p ", "; cg_exp e2; p ")"
    | Len (e, _)                   -> p_ind il; p "len("; cg_exp e; p ")"
    | Cap (e, _)                   -> p_ind il; p "cap("; cg_exp e; p ")"
    | FunctionCall (Identifier (str, pos) as id, es, _) ->
        let cat,glt,dec_pos = dec_lookup str pos in
        p_ind il;
        (if cat = Type then (* type cast *)
            (match rt glt with
            | IntT    -> p "int"
            | FloatT  -> p "float"
            | RuneT   -> p "int"
            | StringT -> p "str"
            | _       -> ())
        else
            cg_id id);
        p "(";
        let f e = (p "cpy("; cg_exp e; p "), ") in
        List.iter f es;
        p ")"

and cg_stmt ?(il=0) stmt =
    match stmt with
    | PrintStatement (eso, _) ->
        p_ind il; p "print(";
        (match eso with
        | Some eso ->
            let f e = cg_exp e; p ", " in
            List.iter f eso
        | None -> ());
        p " end='')\n"
    | PrintlnStatement (eso, _) ->
        p_ind il; p "print (";
        (match eso with
        | Some eso ->
            let f e = cg_exp e; p ", " in
            List.iter f eso
        | None -> ());
        p ")\n"
    | Inc e ->  p_ind il; cg_exp e; p " = "; cg_exp e; p " + 1 \n"
    | Dec e ->  p_ind il; cg_exp e; p " = "; cg_exp e; p " - 1 \n"
    | Break _ -> p_ind il; p "break\n"
    | Continue _ -> p_ind il; p "continue\n"
    | ReturnStatement (eso, _) ->
        p_ind il; p "return ";
        (match eso with
        | Some es -> cg_exp es
        | None -> ());
        p "\n"
    | ExpressionStatement (e, _) -> cg_exp ~il e; p "\n"
    | DeclarationStatement dec -> cg_decl ~il dec
    | AssignmentStatement (es1, ao, es2) ->
        p_ind il;
        let f1 e = cg_exp e; p ", " in
        let f2 e = cg_exp e in
        (match ao with
        | ASG     -> List.iter f1 es1; p "= " ; List.iter f1 es2;
        (* es length = 1 *)
        | PLUSEQ  -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " + "  ; List.iter f2 es2
        | MINUSEQ -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " - "  ; List.iter f2 es2
        | MULTEQ  -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " * "  ; List.iter f2 es2
        | DIVEQ   -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " // " ; List.iter f2 es2
        | MODEQ   -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " % "  ; List.iter f2 es2
        | BANDEQ  -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " & "  ; List.iter f2 es2
        | BOREQ   -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " | "  ; List.iter f2 es2
        | XOREQ   -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " ^ "  ; List.iter f2 es2
        | LSHFTEQ -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " << " ; List.iter f2 es2
        | RSHFTEQ -> List.iter f2 es1; p " = " ; List.iter f2 es1; p " >> " ; List.iter f2 es2
        | NANDEQ  -> List.iter f2 es1; p " = " ; p "~("; List.iter f2 es1; p " & " ; List.iter f2 es2; p ")" );
        p "\n";
    | ShortValDeclaration (ids, es) ->
        p_ind il;
        let f id = cg_id id; p ", " in
        List.iter f ids; p " = ";
        let f e = cg_exp e; p ", " in
        List.iter f es; p "\n"
    | IfStatement ifstmt -> cg_ifstmt ~il ifstmt
    | ForStatement (s1, eo, s2, ss, pos, pos2) ->
        p_ind il; p "def for_statement():\n";

        enter_first_child_scope ();
        get_and_print_nonlocals ~il:(il+1) [s1] pos;

        cg_stmt ~il:(il+1) s1;

        p_ind (il+1); p "def _for():\n";

        enter_first_child_scope ();
        get_and_print_nonlocals ~il:(il+2) (ss@[s2]) (pos2);

        p_ind (il+2); p "while ";
        (match eo with
        | None -> p "True"
        | Some e -> cg_exp e);
        p ":\n";

        List.iter (cg_stmt ~il:(il+3)) (ss@[s2]);

        enter_parent_and_delete_first_child_scope ();
        p_ind (il+1); p "_for()\n";

        enter_parent_and_delete_first_child_scope ();
        p_ind il; p "for_statement()\n"

    | SwitchStatement (s, eo, swcs, pos) ->
        p_ind il; p "def switch_statement():\n";

        enter_first_child_scope ();
        get_and_print_nonlocals ~il:(il+1) [s] pos;
        cg_stmt ~il:(il+1) s;

        p_ind (il+1); p "se = ";
        (match eo with
        | Some e -> cg_exp e
        | None -> p "True");
        p "\n";

        p_ind (il+1); p "if False: pass\n";

        let cg_swc do_case_not_def swc =
            let o =
                (match swc with
                | Case (es, ss, pos) ->
                    if not do_case_not_def then None
                    else Some (es,ss,pos)
                | Default (ss, pos) ->
                    if do_case_not_def then None
                    else Some ([LitBool (true,(-1,-1))], ss, pos)) in
            (match o with
            | None -> ()
            | Some (es, ss, pos) ->
                enter_first_child_scope ();

                p_ind (il+1); p "elif se == any([";
                List.iter (fun e -> cg_exp e; p ", ") es;
                p "]):\n";
                
                p_ind (il+2); p "def switch_case():\n";
                get_and_print_nonlocals ~il:(il+3) ss pos;
                List.iter (cg_stmt ~il:(il+3)) ss;

                enter_parent_and_delete_first_child_scope ();
                p_ind (il+2); p "switch_case()\n") in
        List.iter (cg_swc true) swcs;
        List.iter (cg_swc false) swcs;

        enter_parent_and_delete_first_child_scope ();
        p_ind il; p "switch_statement()\n"

    | BlockStatements (ss, pos) ->
        p "\n"; p_ind il; p "def block_statements():\n";

        enter_first_child_scope ();
        get_and_print_nonlocals ~il:(il+1) ss pos;

        List.iter (cg_stmt ~il:(il + 1)) ss;

        enter_parent_and_delete_first_child_scope ();
        p_ind il; p "block_statements()\n"

    | EmptyStatement -> p "\n"

and cg_ifstmt ?(il=0) (If (s, e, ss, elso, pos)) =
    p_ind il; p "def if_statement():\n";
    enter_first_child_scope ();
    get_and_print_nonlocals ~il:(il+1) [s] pos;

    cg_stmt ~il:(il+1) s;
    p_ind (il + 1); p "if "; cg_exp e; p ":\n";

    p_ind (il + 2); p "def _if():\n";
    enter_first_child_scope ();
    get_and_print_nonlocals ~il:(il+3) ss (get_pos_e e);

    List.iter (cg_stmt ~il:(il + 3)) ss;

    enter_parent_and_delete_first_child_scope ();
    p_ind (il + 2); p "_if()\n";

    (match elso with
    | Some els ->
        p_ind (il + 1); p "else:\n";
        (match els with
        | Elseif (ifstmt,_) -> cg_ifstmt ~il:(il+2) ifstmt
        | Else (ss, pos) ->
            p_ind (il+2); p "def else_statement():\n";
            enter_first_child_scope ();
            get_and_print_nonlocals ~il:(il+3) ss pos;

            List.iter (cg_stmt ~il:(il+3)) ss;

            enter_parent_and_delete_first_child_scope ();
            p_ind (il + 2); p "else_statement()\n";
        )
    | None -> ());

    enter_parent_and_delete_first_child_scope ();
    p_ind il; p "if_statement()\n\n"
