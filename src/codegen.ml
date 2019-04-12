open Tree
open Typecheck
open Helpers
open Cactus_stack
open Golitetypes

(* append positions to variable names for overshadowing *)
(* for every non-declared variable in current scope, use 'nonlocal' on it *)
(* copy on EVERY RHS and ALL FUNC ARGS *)

let p s = print_string s

let p_ind il = p (String.make (il * 4) ' ')

let z (ln,cn) =
    if (ln,cn) = (-2,-2) then "" else
    let f i = if i < 0 then "_"^(string_of_int (-1 * i)) else string_of_int i in
    "_"^(f ln)^"_"^(f cn)

let get_nonlocals ss scope_start =
    let remove_dups xs =
        let rec remove_dups' xs = (* TODO *)
            match xs with
            | h1::(h2::_ as t) ->
                if h1 = h2 then remove_dups' t else
                h1 :: remove_dups' t
            | _ -> in
        remove_dups' (List.sort xs) in
    let f acc s =
        let s_nonlocals =
            match s with (* TODO *)
            | _ -> [""] in
        s_nonlocals @ acc
    remove_dups @@ List.fold_left f [] ss

let rec cg_glt_default ?(il=0) varname glt =
    match glt with
    | Void | FunctionT (_,_) -> raise (CodegenError "HOW YOU DO THIS TO ME?!")
    | IntT                   -> p_ind il; p (varname^" = 0\n")
    | FloatT                 -> p_ind il; p (varname^" = 0.0\n")
    | BoolT                  -> p_ind il; p (varname^" = False\n")
    | RuneT                  -> p_ind il; p (varname^" = 0\n")
    | StringT                -> p_ind il; p (varname^" = \"\"\n")
    | SliceT _               -> p_ind il; p (varname^" = go_slice()\n")
    | NamedT (str, pos)      -> cg_glt_default ~il varname (rt glt)
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

let current_scope = ref global_scope

let dec_lookup str pos = (* get position of declaration for Identifier(str, pos) *)
    let cat,glt,dec_pos = lookup ~current_scope str pos in
    if dec_pos <= pos then cat,glt,dec_pos else
    lookup ~current_scope:(ref (parent !current_scope)) str pos

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
        p "main()\n\n"
    | EmptyProgram -> ()

and cg_decl ?(il=0) dec =
    match dec with
    | FunctionDeclaration (id, para, tso, ss, pos) -> () (* TODO *)
      (*cg_ident id; cg_para para; List.iter cg_stmt; cg_type tso*)
    | VariableDeclaration (vds, pos) ->
        let cg_vd (ids, tso, eso) =
            let eos =
                (match eso with
                | Some es -> List.map (fun e -> Some e) es
                | None    -> List.init (List.length ids) (fun i -> None)) in
            let glts_pos =
                let f (Identifier (str, pos)) =
                    let (_, glt, pos) = lookup ~current_scope str pos in
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
    if str = "_" then p (z pos) else
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
    | Append (e1, e2, _)           -> p_ind il; p "append("; cg_exp e1; p ", "; cg_exp e2; p ")\n"
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
        p "print (";
        (match eso with
        | Some eso ->
            let f e = cg_exp e; p ", " in
            List.iter f eso
        | None -> ());
        p ")\n"
    | Inc e ->  cg_exp e; p " = "; cg_exp e; p " + 1 \n"
    | Dec e ->  cg_exp e; p " = "; cg_exp e; p " - 1 \n"
    | Break _ -> p "break\n"
    | Continue _ -> p "continue\n"
    | ReturnStatement (eso, _) ->
        p "return ";
        (match eso with
        | Some es -> cg_exp es
        | None -> ());
        p "\n"
    | ExpressionStatement (e, _) -> cg_exp e; p "\n"
    | DeclarationStatement dec -> cg_decl dec
    | AssignmentStatement (es1, ao, es2) -> ()
    | ShortValDeclaration (ids, es) -> ()
    | IfStatement ifstmt -> () (*TODO *)
    | ForStatement (s1, eso, s2, ss, _) -> () (*TODO*)
    | SwitchStatement (s, eso, swcl, _) -> () (*TODO*)
    | BlockStatements (ss, pos) -> ()
    | EmptyStatement -> ()
