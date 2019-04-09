open Tree
open Helpers
open Cactus_stack
module T = Golitetypes

(* DATA STRUCTURES ------------------------------------------------------------------------------------ *)

let root_context =
    (*        name     kind       type      pos *)
    let ht : (string, (T.vcat * T.gltype * (int*int))) Hashtbl.t = Hashtbl.create 8 in
    (* builtin types *)
    Hashtbl.add ht "int"     (T.Type,     T.IntT,                     (-1,-1));
    Hashtbl.add ht "float64" (T.Type,     T.FloatT,                   (-1,-1));
    Hashtbl.add ht "bool"    (T.Type,     T.BoolT,                    (-1,-1));
    Hashtbl.add ht "rune"    (T.Type,     T.RuneT,                    (-1,-1));
    Hashtbl.add ht "string"  (T.Type,     T.StringT,                  (-1,-1));
    (* literals *)
    Hashtbl.add ht "true"    (T.Constant, T.NamedT ("bool", (-1,-1)), (-1,-1));
    Hashtbl.add ht "false"   (T.Constant, T.NamedT ("bool", (-1,-1)), (-1,-1));
    ht

let root_scope = CsNode {
    parent = CsRoot;
    children = ref [];
    context = root_context;
}

let global_scope = CsNode {
    parent = root_scope;
    children = ref [];
    context = Hashtbl.create 8;
}

let () =
    match root_scope with
    | CsRoot -> raise (TypeCheckError ("IMPOSSIBLE", (-1,-1)))
    | CsNode { parent; children; context } ->
        children := global_scope :: !children

(* initialized to global scope, the only child of the root scope *)
let current_scope = ref global_scope

(* HELPERS -------------------------------------------------------------------------------------------- *)

let is_IntT t    = match t with | T.IntT -> true | _ -> false
let is_FloatT t  = match t with | T.FloatT -> true | _ -> false
let is_BoolT t   = match t with | T.BoolT -> true | _ -> false
let is_bool t    = match t with | T.NamedT ("bool", (-1,-1)) -> true | _ -> false
let is_RuneT t   = match t with | T.RuneT -> true | _ -> false
let is_StringT t = match t with | T.StringT -> true | _ -> false
let is_StructT t = match t with | T.StructT _ -> true | _ -> false
let is_SliceT t  = match t with | T.SliceT _ -> true | _ -> false
let is_ArrayT t  = match t with | T.ArrayT (_,_) -> true | _ -> false
let is_FunctionT t = match t with | T.FunctionT (_,_) -> true | _ -> false

let is_Ts fs t = List.exists (fun f -> f t) fs
let is_intT = is_Ts [is_IntT; is_RuneT]
let is_numT = is_Ts @@ is_intT :: [is_FloatT]
let is_ordT = is_Ts @@ is_numT :: [is_StringT]
let is_basT = is_Ts @@ is_ordT :: [is_BoolT]
let is_cmpT = is_Ts @@ is_ordT :: [is_BoolT; is_StructT; is_ArrayT]
let intmsg  = "IntT, RuneT"
let nummsg  = "IntT, RuneT, FloatT"
let ordmsg  = "IntT, RuneT, FloatT, StringT"
let basmsg  = "IntT, RuneT, FloatT, StringT, BoolT"
let cmpmsg  = "IntT, RuneT, FloatT, StringT, BoolT, StructT, ArrayT"
let sctmsg  = "StructT"

let lookup ?(current_scope=current_scope) str pos =
    let rec lookup' str pos scope =
        try Hashtbl.find (context scope) str with
        | Not_found ->
            (match parent scope with
            | CsRoot -> raise (TypeCheckError (str ^ " not declared", pos))
            | csn    -> lookup' str pos csn) in
    lookup' str pos !current_scope

let rt glt =
    let rec rt' glt scope =
        match glt with
        | T.NamedT (str, pos) ->
            (try rt' (snd3 (Hashtbl.find (context scope) str)) scope with
            | Not_found ->
                (match parent scope with
                | CsRoot -> raise (TypeCheckError (str^" not declared", pos))
                | csn    -> rt' glt csn))
        | _ -> glt in
    rt' glt !current_scope

(* Pass-Through [y] If Resolved Type [of y is] In [list of types of f] *)
let pt_if_rt f msg (y, pos) =
    if f (rt y) then y
    else raise (TypeCheckError ("RT( "^(T.string_of_glt y)^" ) not in "^msg, pos))

let pt_if f msg (y, pos) =
    if f y then y
    else raise (TypeCheckError ((T.string_of_glt y)^" is not in "^msg, pos))

(* Pass-Through [y] If Resolved Type [of y is] is RECURSIVELY never T.SliceT *)
let pt_if_never_slice (y, pos) =
    let rec pt_if_never_slice' y' =
        match y' with
        | T.SliceT _        -> raise (TypeCheckError ("slice is not comparable", pos))
        | T.ArrayT (i, glt) -> pt_if_never_slice' glt
        | T.StructT stds    -> let _,glts = List.split stds in List.hd @@ List.map pt_if_never_slice' glts
        | T.NamedT _        -> pt_if_never_slice' (rt y')
        | _                 -> (y, pos) in
    pt_if_never_slice' y

let err_if_id_in_current_scope (Identifier (str, pos)) =
    try
        let _ = Hashtbl.find (context !current_scope) str in
        raise (TypeCheckError (str ^ " is already in current scope", pos))
    with
    | Not_found -> ()

(* recurse until entry found, then do check *)
let err_if_id_not_declared ?f:(op=0) ?(check=(fun a b -> b)) (Identifier (str, pos)) =
    let rec err_if_id_not_declared' str scope =
        try
            let (cat, glt, pos) = Hashtbl.find (context scope) str in
            check cat glt
        with
        | Not_found ->
            (match op with 
            | 0 -> (match parent scope with
                    | CsRoot -> raise (TypeCheckError (str ^ " not declared", pos))
                    | csn    -> err_if_id_not_declared' str csn)
            | 1 -> T.Void )
   in
   err_if_id_not_declared' str !current_scope

let err_if_type_not_declared (IdentifierType ((Identifier (str, pos)) as id)) =
    let check cat glt =
        if cat = T.Type then glt
        else
            let scat = T.string_of_vcat cat in
            let s = (str ^ " is a " ^ scat ^ ", not a Type") in
            raise (TypeCheckError (s, pos))
    in
    err_if_id_not_declared ~check id

let create_and_enter_child_scope () =
    let add new_scope =
        match !current_scope with
        | CsRoot -> raise (TypeCheckError ("IMPOSSIBLE", (-1, -1)))
        | CsNode {parent; children; context} ->
            children := new_scope :: !children in
    let new_scope = CsNode { parent=(!current_scope); children=ref []; context=(Hashtbl.create 8)} in
    add new_scope; current_scope := new_scope

let enter_parent_scope () =
    match !current_scope with
    | CsRoot -> raise (TypeCheckError ("IMPOSSIBLE", (-1, -1)))
    | CsNode {parent; children; context } -> current_scope := parent


(* TYPE CHECKER ------------------------------------------------------------------------------------------------- *)

let rec type_check_prog prog =
    match prog with
    | EmptyProgram      -> ()
    | Program (pkg, ds) -> List.iter type_check_decl ds

and type_check_decl d =
    match d with
    | VariableDeclaration (vds, pos)                 -> List.iter (fun vd -> type_check_vd vd pos) vds
    | TypeDeclaration (tds, pos)                     -> List.iter type_check_td tds
    | FunctionDeclaration (id, prms, tso, ss, pos) -> type_check_fd(id, prms, tso, ss)

and type_check_vd (ids, tso, eso) pos =
    
    (if not (dup_exists ids) then () else
            let Identifier (str,pos) = find_dup ids in
            raise (TypeCheckError ("duplicate ids: "^str, pos)));
    let f (Identifier (str, pos)) =
        match str with
        | "main" | "init" -> raise (TypeCheckError ("variables cannot be named 'init' or 'main'", pos))
        | _ -> err_if_id_in_current_scope (Identifier (str, pos)); () in
    List.iter f ids;
    
    let glt =
        match tso, eso with
        | None, None -> (* not possible, parser prevents *)
            raise (TypeCheckParserError ("variable declaration requires type of initial value", pos))
        | Some ts, None ->
            type_check_ts ts
        | None, Some es ->
            let e_glts = List.map type_check_e es in
            let h = List.hd e_glts in
            if (let bs = List.map ((=) h) e_glts in
               List.fold_left (fun acc b -> (acc && b)) true bs)
            then h
            else raise (TypeCheckError ("vardec: explist contains multiple types", pos))
        | Some ts, Some es ->
            let t_glt = type_check_ts ts in
            let e_glts = List.map type_check_e es in
            if (let bs = List.map ((=) t_glt) e_glts in
               List.fold_left (fun acc b -> (acc && b)) true bs)
            then t_glt
            else raise (TypeCheckError ("vardec: explist type != declared type", pos)) in

    let add_id_to_scope (Identifier (str, pos)) =
        if str = "_" then () else Hashtbl.add (context !current_scope) str (T.Variable, glt, pos) in
    List.iter add_id_to_scope ids

and type_check_ts ts = (* return the checked type *)
    match ts with
    | IdentifierType (Identifier (str, pos)) as idt ->
        err_if_type_not_declared idt;
        let pos = trd3 (lookup str pos) in 
        T.NamedT (str, pos)
    | ArrayTypeLiteral (es, ts, pos) ->
        (match es with
        | LitInt (i, _) -> ArrayT (i, type_check_ts ts)
        | _  -> raise (TypeCheckParserError ("array needs int within '[ ]'", pos)))
    | SliceTypeLiteral (ts, pos) ->
        SliceT (type_check_ts ts)
    | StructTypeLiteral (stds, pos) ->
        let ids, tss = List.split stds in
        let strs = (List.map (fun (Identifier (str, pos)) -> str) ids) in
        if dup_exists strs then raise (TypeCheckError ("duplicate field names in struct", pos)) else ();
        let type_check_std (Identifier (str, pos), ts) =
            (str, (type_check_ts ts)) in
        StructT (List.map type_check_std stds)

and type_check_td (((Identifier (str, pos))), ts) =
    (match str with 
    | "main" | "init" -> raise (TypeCheckError ("types cannot be named 'init' or 'main'", pos))
    | _ -> err_if_id_in_current_scope (Identifier (str, pos)));
    let glt = type_check_ts ts in
    Hashtbl.add (context !current_scope) str (T.Type, glt, pos)

and type_check_fd (Identifier (str, pos) as id, Parameters prms, tso, ss) =
    (match Identifier (str, pos) with 
    | Identifier ("init", _) -> 
        (match prms, tso with
        |  [] , None -> () 
        | _, _ -> raise (TypeCheckError ("init functions must have no args and return void", pos)));
        ()
    | Identifier ("main", _) ->
        (match prms , tso with
        |  [] , None -> err_if_id_in_current_scope id; () 
        | _, _ -> raise (TypeCheckError ("main function must have no args and return void", pos)));
        ()
    | _ ->  err_if_id_in_current_scope id; ());
    
    let idss, tss = List.split prms in
    let prm_types = List.map type_check_ts tss in
    let ret_type = match tso with | None -> T.Void | Some ts -> type_check_ts ts in
    if str = "_" then () 
    else Hashtbl.add (context !current_scope) str (T.Variable, T.FunctionT (prm_types, ret_type), pos);
    create_and_enter_child_scope ();

    let add_ids_to_scope (ids, glt) =
        let f ((Identifier (str, pos)) as id) =
            if str = "_" then ()
            else err_if_id_in_current_scope id;
            Hashtbl.add (context !current_scope) str (T.Variable, glt, pos) in
        List.iter f ids in
    List.iter add_ids_to_scope (List.combine idss prm_types);

    (match type_check_stmts ss ret_type with
    | None ->
        if ret_type = T.Void then ()
        else raise (TypeCheckError ("function is not void but lacks correct return statement", pos))
    | _ -> ());
    
    enter_parent_scope();

and type_check_e e =
    match e with
    (* literals *)
    (* LET THE POS FOR THE NAMED TYPE FOR LITERALS BE (-1, -1) *)
    | LitInt (i, _)       -> NamedT ("int", (-1, -1))
    | LitFloat (f, _)     -> NamedT ("float64", (-1, -1))
    | LitBool (b, _)      -> NamedT ("bool", (-1, -1))
    | LitRune (r, _)      -> NamedT ("rune", (-1, -1))
    | LitString (s, _)    -> NamedT ("string", (-1, -1))
    | LitRawString (s, _) -> NamedT ("string", (-1, -1))
    (* identifier expression *)
    | IdentifierExpression idexp ->  type_check_idexp idexp
    (* unary expression *)
    | Uplus (e, pos)  -> ((type_check_e e), (get_pos_e e)) |> (pt_if_rt is_numT nummsg)
    | Uminus (e, pos) -> ((type_check_e e), (get_pos_e e)) |> pt_if_rt is_numT nummsg
    | Uxor (e, pos)   -> ((type_check_e e), (get_pos_e e)) |> pt_if_rt is_intT intmsg
    | Not (e, pos)    -> ((type_check_e e), (get_pos_e e)) |> pt_if_rt is_BoolT "BoolT"
    (* binary expression *)
    | Or (e1, e2, pos)    -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_BoolT "BoolT")
    | And (e1, e2, pos)   -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_BoolT "BoolT")
    | Eq (e1, e2, pos)    -> (((pt_if_type_check_eq e1 e2, get_pos_e e1) |> pt_if_rt is_cmpT cmpmsg), get_pos_e e1) |> pt_if_never_slice |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Neq (e1, e2, pos)   -> (((pt_if_type_check_eq e1 e2, get_pos_e e1) |> pt_if_rt is_cmpT cmpmsg), get_pos_e e1) |> pt_if_never_slice |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Gt (e1, e2, pos)    -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_ordT ordmsg) |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Gteq (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_ordT ordmsg) |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Lt (e1, e2, pos)    -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_ordT ordmsg) |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Lteq (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_ordT ordmsg) |> (fun x -> T.NamedT ("bool", (-1,-1)))
    | Plus (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_ordT ordmsg)
    | Minus (e1, e2, pos) -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_numT nummsg)
    | Mult (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_numT nummsg)
    | Div (e1, e2, pos)   -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_numT nummsg)
    | Mod (e1, e2, pos)   -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Bor (e1, e2, pos)   -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Band (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Xor (e1, e2, pos)   -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Nand (e1, e2, pos)  -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Lshft (e1, e2, pos) -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    | Rshft (e1, e2, pos) -> (pt_if_type_check_eq e1 e2, get_pos_e e1) |> (pt_if_rt is_intT intmsg)
    (* function calls *)
    | Append (e1, e2, pos) ->
        let t = type_check_e e1 in
        (match rt t with
        | T.SliceT glt -> if glt = type_check_e e2 then t else raise (TypeCheckError ("appended type != slice type", pos))
        | _ -> raise (TypeCheckError ("cannot append to non-slice", pos)))
    | Cap (e, pos) ->
        (match rt (type_check_e e) with
        | T.ArrayT (_,_) | T.SliceT _ -> T.NamedT ("int", (-1,-1))
        | _                           -> raise (TypeCheckError ("cap only defined for array, slice", pos)))
    | Len (e, pos) ->
        (match rt (type_check_e e) with
        | T.ArrayT (_,_) | T.SliceT _ | T.StringT -> T.NamedT ("int", (-1,-1))
        | _                                       -> raise (TypeCheckError ("len only defined for array, slice, string", pos)))
    | FunctionCall ((Identifier (str, pos1)), es, pos2) ->
        let cat, glt, pos = lookup str pos1 in
        (match cat with
        | T.Variable ->
            (match glt with
            | T.FunctionT (arg_glts, ret_glt) ->
                err_if (arg_glts <> List.map type_check_e es) (TypeCheckError ("arg types given != expected", pos1)); ret_glt
            | _ -> raise (TypeCheckError (str^" is not a function", pos1)))
        | T.Type -> (* type cast *)
            if (List.length es) > 1 then raise (TypeCheckError ("can only cast one value at a time", pos1)) else ();
            let rt_caster, rt_castee = (rt glt), (rt (type_check_e (List.hd es))) in
            ignore (List.map (pt_if_rt is_basT basmsg) [(rt_caster,pos1); (rt_castee,get_pos_e (List.hd es))]);
            (match rt_caster, rt_castee with
            | T.StringT,_ when is_intT rt_castee                      -> T.NamedT (str, pos)
            | _,        _ when is_numT rt_caster && is_numT rt_castee -> T.NamedT (str, pos)
            | _,        _ when rt_caster = rt_castee                  -> T.NamedT (str, pos)
            | _,_ -> raise (TypeCheckError ("invalid type cast", pos1))))
    (* parentheses *)
    | ParenExpression (e, pos) -> type_check_e e

and pt_if_type_check_eq e1 e2 =
    let pos1 = get_pos_e e1 in
    let t1, t2 = (type_check_e e1), (type_check_e e2) in
    if t1 = t2 then t1
    else
        let s1, s2 = (T.string_of_glt t1),(T.string_of_glt t2) in
        raise (TypeCheckError (s1 ^ " not equal to " ^ s2, pos1))

and type_check_idexp idexp =
    match idexp with
    | Ident (str, pos) ->
        (let cat,glt,_ = (lookup str pos) in
        (match glt with 
        | FunctionT (para, ret) -> raise (TypeCheckError ("functions cannot be used as variables", pos))
        | _ -> if cat=T.Type then raise (TypeCheckError ("types are not variables", pos)) else ());
        glt)
    | Indexed (e1, e2, pos) ->
        (type_check_e e2, get_pos_e e2) |> (pt_if_rt is_intT intmsg) |> (fun x -> ());
        (match rt (type_check_e e1) with
        | T.SliceT glt | T.ArrayT (_, glt) -> glt
        | _ -> raise (TypeCheckError ("can only index into slices and arrays", pos)))
    | StructAccess (e, str, pos) ->
        (match ((type_check_e e, get_pos_e e) |> (pt_if_rt is_StructT sctmsg) |> rt) with
        | T.StructT stds ->
            let f acc (str',glt) =
                if acc <> None then acc else
                if str=str' then Some glt else None in
            let glto = List.fold_left f None stds in
            (match glto with
            | Some glt -> glt
            | None     -> raise (TypeCheckError ("struct does not have field "^str, pos)))
        | _  -> raise (TypeCheckError ("cannot access field of non-struct", pos)))

and type_check_stmt s outer_ret_glt =
    match s with
    | ExpressionStatement (e, pos) -> type_check_e e; None
    | AssignmentStatement (es1, aop, es2) ->
        let pos = get_pos_e (List.hd es1) in
        if (List.length es1)<>(List.length es2) then
        raise (TypeCheckError ("multiple assignment size mismatch", pos)) else ();
        let f (e1,e2) =
            let pos = get_pos_e e1 in
            (match e1 with 
            | IdentifierExpression idexp ->
                (match idexp with
                | Ident (str, pos) when str="_" -> type_check_e e2; ()
                | _ ->
                    if (type_check_e e1) <> (type_check_e e2) then
                    let pos = get_pos_e e1 in
                    raise (TypeCheckError ("assignment type mismatch", pos)) else ())
            | _ -> raise (TypeCheckError ("invalid assignment left hand side", pos))) in
        List.iter f (List.combine es1 es2);
        if (List.length es1 <> 1) && (aop <> ASG) then
        let pos = get_pos_e (List.hd es1) in
        raise (TypeCheckError ("cannot use shorthand operators in multiple assignment", pos)) else ();
        None
    | ReturnStatement (eo, pos) -> 
        let glt =
            (match eo with 
            | None -> T.Void
            | Some e -> type_check_e e) in
        if glt = outer_ret_glt then Some glt
        else raise (TypeCheckError ("returned type doesn't not match declared return type", pos))
    | ShortValDeclaration (ids, es) -> 
        (if (List.length ids) = (List.length es) then () else
            let Identifier (_,pos) = List.hd ids in
            raise (TypeCheckError ("multiple assignment size mismatch", pos)));
        (if not (dup_exists ids) then () else
            let Identifier (str,pos) = find_dup ids in
            raise (TypeCheckError ("duplicate ids: "^str, pos)));
        List.iter (fun e -> type_check_e e; ()) es;
        let if_at_least_one = ref 0 in
        (let f (Identifier (str,pos) as id, e) = 
            let id_type  = 
                let check cat glt = glt in
                    err_if_id_not_declared ~f:1 ~check:check id in
            match id_type with 
            | T.Void -> 
                if str = "_" then () else 
                (if_at_least_one := 1;
                let glt = type_check_e e in ();
                (match glt with 
                | T.Void -> raise (TypeCheckError ("void type cannot be assigned (to "^str^")", pos))
                | _ -> Hashtbl.add (context !current_scope) str (T.Variable, glt, pos));); 
                ();
            | _ as glt ->
                if glt <> type_check_e e then 
                    raise (TypeCheckError ("assignment type mismatch", pos))
                else () in
        List.iter f (List.combine ids es) );
        if !if_at_least_one <> 0 then None else
        let Identifier (_,pos) = List.hd ids in
        raise (TypeCheckError ("all ids in shorthand-assignment LHS cannot be already declared", pos))
    | BlockStatements (ss, pos) ->
        create_and_enter_child_scope ();
        let glto = type_check_stmts ss outer_ret_glt in
        enter_parent_scope();
        glto
    | DeclarationStatement d -> type_check_decl d; None
    | Inc e | Dec e -> (type_check_e e, get_pos_e e) |> (pt_if_rt is_numT nummsg); None
    | PrintStatement (es, pos) | PrintlnStatement (es, pos) ->
        (match es with
        | None -> ()
        | Some es ->
            let f e = ignore( (type_check_e e, get_pos_e e) |> (pt_if_rt (is_Ts (is_ordT :: [is_BoolT])) "Cannot print") ) in
            List.iter f es);
        None
    | Break _ | Continue _ | EmptyStatement -> None
    | ForStatement (s1, eo, s2, ss, pos)  -> type_check_for (s1, eo, s2, ss) outer_ret_glt
    | IfStatement ifclause -> type_check_ifst ifclause outer_ret_glt
    | SwitchStatement (s, eo, scl, pos) -> type_check_switch (s, eo, scl) outer_ret_glt
    | _ -> raise (TypeCheckError ("IMPOSSIBLE", (-1,-1)))

and type_check_for f outer_ret_glt =
    match f with
    | (EmptyStatement, None, EmptyStatement, ss) ->
        create_and_enter_child_scope ();
        let glto = type_check_stmts ss outer_ret_glt in 
        enter_parent_scope();
        glto
    | (EmptyStatement, Some e, EmptyStatement, ss) ->
        (type_check_e e, get_pos_e e) |> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope();
        let glto = type_check_stmts ss outer_ret_glt in
        enter_parent_scope();
        glto
    | (i, Some e, p , ss) ->
        create_and_enter_child_scope ();
        type_check_stmt i outer_ret_glt;
        (type_check_e e, get_pos_e e) |> pt_if_rt is_BoolT "BoolT";
        type_check_stmt p outer_ret_glt;
        create_and_enter_child_scope () ;
        let glto = type_check_stmts ss outer_ret_glt in 
        enter_parent_scope();
        enter_parent_scope();
        glto
    | (i, None , EmptyStatement , ss) ->
        create_and_enter_child_scope();
        type_check_stmt i outer_ret_glt;
        create_and_enter_child_scope();
        let glto = type_check_stmts ss outer_ret_glt in 
        enter_parent_scope();
        enter_parent_scope();
        glto
    | _ -> raise (TypeCheckError ("IMPOSSIBLE", (-1,-1)))

and type_check_ifst ic outer_ret_glt =
    match ic with
    | If (s, e, ss, None, pos) ->
        create_and_enter_child_scope();
        type_check_stmt s outer_ret_glt;
        (type_check_e e, get_pos_e e) |> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope ();
        let glto = type_check_stmts ss outer_ret_glt in 
        enter_parent_scope();
        enter_parent_scope();
        glto
        
    | If (s, e, ss, Some els, pos) ->
        create_and_enter_child_scope();
        type_check_stmt s outer_ret_glt;
        (type_check_e e, get_pos_e e) |> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope();
        let glto1 = type_check_stmts ss outer_ret_glt in 
        enter_parent_scope();
        let glto2 =
            (match els with
            | Elseif (ifs, pos) -> type_check_ifst ifs outer_ret_glt
            | Else (ss, pos)->
                create_and_enter_child_scope();
                let glto2 = type_check_stmts ss outer_ret_glt in 
                enter_parent_scope();
                glto2) in
        enter_parent_scope();
        if glto1 <> glto2 then None else glto1
       
and type_check_switch sw outer_ret_glt =
    match sw with
    | (s, eo , swcs) ->
        create_and_enter_child_scope ();
        type_check_stmt s outer_ret_glt;
        (match eo with
        | None   -> ()
        | Some e -> (type_check_e e, get_pos_e e) |> pt_if_rt is_cmpT cmpmsg |> ignore);
        let f swc =
            (match swc with
            | Default (ss, pos) ->
                create_and_enter_child_scope ();
                let glto = type_check_stmts ss outer_ret_glt in 
                enter_parent_scope ();
                glto,true
            | Case (es, ss, pos) ->
                (match eo with
                | None ->
                    let f e =
                        (type_check_e e, get_pos_e e) |> pt_if is_bool "bool" |> ignore in
                    List.iter f es
                | Some e -> List.iter (fun e2 -> pt_if_type_check_eq e e2; ()) es);
                create_and_enter_child_scope ();
                let glto = type_check_stmts ss outer_ret_glt in 
                enter_parent_scope ();
                glto,false) in
        let gltos_isdefs = List.map f swcs in
        enter_parent_scope ();
        let f (last_glt,hasdef) (glto,isdef) =
            (match (glto, isdef) with 
            | (None, _) -> (None, if isdef then isdef else hasdef)
            | (_, true) -> let new_glt = (if hasdef = false then None else glto) in (new_glt, hasdef)
            | (_, false) -> (last_glt, hasdef))
            in
        let (glt, hasdef) = (List.fold_left f (None,true) gltos_isdefs) in 
        if hasdef = true then glt else None
   
and type_check_stmts ss outer_ret_glt =
    let last_i = (List.length ss)-1 in
    let f (i,glto) s =
        let curr_glto = type_check_stmt s outer_ret_glt in
        if i <> last_i && curr_glto <> None then
            let pos =
                match s with
                | IfStatement (If (_, _, _, _, pos)) -> pos
                | SwitchStatement (_, _, _, pos) -> pos
                | ForStatement (_, _, _, _, pos) -> pos
                | BlockStatements (_, pos) -> pos
                | ReturnStatement (_, pos) -> pos
                | _ -> raise (TypeCheckError ("IMPOSSIBLE", (-1,-1))) in
            raise (TypeCheckError ("unreachable code, return statement too early", pos))
        else (i+1,curr_glto) in
    snd (List.fold_left f (0,None) ss)
