open Tree
open Helpers
open Cactus_stack
module T = Golitetypes

(* DATA STRUCTURES ------------------------------------------------------------------------------------ *)

let root_context =
    let ht : (string, (T.vcat * T.gltype)) Hashtbl.t = Hashtbl.create 8 in
    (* builtin types *)
    Hashtbl.add ht "int"     (T.Type,     T.IntT);
    Hashtbl.add ht "float64" (T.Type,     T.FloatT);
    Hashtbl.add ht "bool"    (T.Type,     T.BoolT);
    Hashtbl.add ht "rune"    (T.Type,     T.RuneT);
    Hashtbl.add ht "string"  (T.Type,     T.StringT);
    (* literals *)
    Hashtbl.add ht "true"    (T.Constant, T.NamedT ("bool", (-1,-1)));
    Hashtbl.add ht "false"   (T.Constant, T.NamedT ("bool", (-1,-1)));
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

let curr_ret_val = ref T.Void 

let tag_ret = ref 0
let temp_ret = ref T.Void  

(* HELPERS -------------------------------------------------------------------------------------------- *)

let is_IntT t    = match t with | T.IntT -> true | _ -> false
let is_FloatT t  = match t with | T.FloatT -> true | _ -> false
let is_BoolT t   = match t with | T.BoolT -> true | _ -> false
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

let get_vcat_gltype_from_str str pos =
    let rec get_vcat_gltype_from_str' str pos scope =
        try Hashtbl.find (context scope) str with
        | Not_found ->
            (match parent scope with
            | CsRoot -> raise (TypeCheckError (str ^ " not declared", pos))
            | csn    -> get_vcat_gltype_from_str' str pos csn) in
    get_vcat_gltype_from_str' str pos !current_scope

let rt glt =
    let rec rt' glt scope =
        match glt with
        | T.NamedT (str, pos) ->
            (try rt' (snd (Hashtbl.find (context scope) str)) scope with
            | Not_found ->
                (match parent scope with
                | CsRoot -> raise (TypeCheckError (str^" not declared", pos))
                | csn    -> rt' glt csn))
        | _ -> glt in
    rt' glt !current_scope

(* Pass-Through [y] If Resolved Type [of y is] In [list of types of f] *)
let pt_if_rt f msg (y, pos) =
    if f (rt y) then (y, pos)
    else raise (TypeCheckError ("RT( "^(T.string_of_glt y)^" ) not in "^msg, pos))

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
let err_if_id_not_declared ?f:(op=0) ?(check=(fun a b -> b)) (Identifier str) =
    let rec err_if_id_not_declared' str scope =
        try
            let (cat, glt) = Hashtbl.find (context scope) str in
            check cat glt
        with
        | Not_found ->
            (match op with 
            | 0 -> (match parent scope with
                    | CsRoot -> raise (TypeCheckError (str ^ " not declared"))
                    | csn    -> err_if_id_not_declared' str csn)
            | 1 -> T.Void )
   in
   err_if_id_not_declared' str !current_scope

let err_if_type_not_declared (IdentifierType ((Identifier str) as id)) =
    let check cat glt =
        if cat = T.Type then glt
        else
            let scat = T.string_of_vcat cat in
            let s = (str ^ " is a " ^ scat ^ ", not a Type") in
            raise (TypeCheckError s)
    in
    err_if_id_not_declared ~check id

let create_and_enter_child_scope () =
    let add new_scope =
        match !current_scope with
        | CsRoot -> raise (TypeCheckError "IMPOSSIBLE")
        | CsNode {parent; children; context} ->
            children := new_scope :: !children in
    let new_scope = CsNode { parent=(!current_scope); children=ref []; context=(Hashtbl.create 8)} in
    add new_scope; current_scope := new_scope

let enter_parent_scope () =
    match !current_scope with
    | CsRoot -> raise (TypeCheckError "IMPOSSIBLE")
    | CsNode {parent; children; context } -> current_scope := parent


(* TYPE CHECKER ------------------------------------------------------------------------------------------------- *)

let rec type_check_prog prog =
    match prog with
    | EmptyProgram      -> ()
    | Program (pkg, ds) -> List.iter type_check_decl ds

and type_check_decl d =
    match d with
    | VariableDeclaration vds                 -> List.iter type_check_vd vds
    | TypeDeclaration tds                     -> List.iter type_check_td tds
    | FunctionDeclaration (id, prms, tso, ss) -> type_check_fd(id, prms, tso, ss)

and type_check_vd (ids, tso, eso) =
    List.iter (fun (Identifier str) -> 
                        (match str with 
                        | "main" | "init" -> raise (TypeCheckError "init and main cannot be used for var declarations")
                        | _ -> err_if_id_in_current_scope (Identifier str)); ();) ids;
    let glt =
        match tso, eso with
        | None, None -> (* not possible, parser prevents *)
            raise (TypeCheckParserError "tso and eso both None")
        | Some ts, None ->
            type_check_ts ts
        | None, Some es ->
            let e_glts = List.map type_check_e es in
            let h = List.hd e_glts in
            if (let bs = List.map ((=) h) e_glts in
               List.fold_left (fun acc b -> (acc && b)) true bs)
            then h
            else raise (TypeCheckError "vardec: explist contains multiple types")
        | Some ts, Some es ->
            let t_glt = type_check_ts ts in
            let e_glts = List.map type_check_e es in
            if (let bs = List.map ((=) t_glt) e_glts in
               List.fold_left (fun acc b -> (acc && b)) true bs)
            then t_glt
            else raise (TypeCheckError "vardec: explist type != declared type") in
    let add_id_to_scope (Identifier str) =
        if str = "_" then () else Hashtbl.add (context !current_scope) str (T.Variable, glt) in
    List.iter add_id_to_scope ids

and type_check_ts ts = (* return the checked type *)
    match ts with
    | IdentifierType (Identifier str) as idt ->
        err_if_type_not_declared idt;
        T.NamedT str
    | ArrayTypeLiteral (es, ts) ->
        (match es with
        | LitInt i -> ArrayT (i, type_check_ts ts)
        | _        -> raise (TypeCheckParserError "array needs int within '[ ]'"))
    | SliceTypeLiteral ts ->
        SliceT (type_check_ts ts)
    | StructTypeLiteral stds ->
        let strs, tss = List.split stds in
        if dup_exists strs then raise (TypeCheckError "duplicate field names") else ();
        let type_check_std (Identifier str, ts) =
            (str, (type_check_ts ts)) in
        StructT (List.map type_check_std stds)

and type_check_td (((Identifier str) as id), ts) =
    (match str with 
    | "main" | "init" -> raise (TypeCheckError "init and main cannot be used for type declarations")
    | _ -> err_if_id_in_current_scope (Identifier str));
    let glt = type_check_ts ts in
    Hashtbl.add (context !current_scope) str (T.Type, glt)

and type_check_fd (Identifier str as id, Parameters prms, tso, ss) =
   
    (match Identifier str with 
    | Identifier "init" -> 
        (match prms , tso with
        |  [] , None -> () 
        | _, _ -> raise (TypeCheckError "Init and main types should not have return or any parameters") ); ()
    | Identifier "main" ->
        (match prms , tso with
        |  [] , None -> err_if_id_in_current_scope id; () 
        | _, _ -> raise (TypeCheckError "Init and main types should not have return or any parameters") ); ()
    | _ ->  err_if_id_in_current_scope id; ());
    
    let idss, tss = List.split prms in
    let prm_types = List.map type_check_ts tss in
    let ret_type = match tso with | None -> T.Void | Some ts -> type_check_ts ts in
    Hashtbl.add (context !current_scope) str (T.Variable, T.FunctionT (prm_types, ret_type));
    create_and_enter_child_scope ();
    curr_ret_val := ret_type;
    let add_ids_to_scope (ids, glt) =
        List.iter (fun (Identifier str) -> if str = "_" then () else err_if_id_in_current_scope (Identifier str); Hashtbl.add (context !current_scope) str (T.Variable, glt)) ids in
    List.iter add_ids_to_scope (List.combine idss prm_types);
    (*List.iter (fun s -> type_check_stmt s; ()) ss;*)
    if ret_type <> !(type_check_stmts ss) then 
       (if ((!temp_ret = ret_type) && (!tag_ret = 0)) then (temp_ret := T.Void;) 
        else raise (TypeCheckError "Return type not match"))             
    (*Now it matches the return type --> Now to check if there is any conflict of return types *)
    else if !tag_ret = 2 then
        (tag_ret := 0;
        temp_ret := T.Void; 
        raise (TypeCheckError "Return type not match"))    
    else if !tag_ret = 1 then 
        ( tag_ret := 0;
        temp_ret := T.Void; )
    else if !tag_ret = 3 then
        (tag_ret := 0;
        temp_ret := T.Void;
        raise (TypeCheckError "Return types at places do not match the return type of fucntion");)
    else ();
    
    enter_parent_scope();

and type_check_e e =
    match e with
    (* literals *)
    | LitInt i -> NamedT "int"
    | LitFloat f -> NamedT "float64"
    | LitBool b -> NamedT "bool"
    | LitRune r -> NamedT "rune"
    | LitString s -> NamedT "string"
    | LitRawString s -> NamedT "string"
    (* identifier expression *)
    | IdentifierExpression idexp -> type_check_idexp idexp
    (* unary expression *)
    | Uplus e  -> type_check_e e |> pt_if_rt is_numT nummsg
    | Uminus e -> type_check_e e |> pt_if_rt is_numT nummsg
    | Uxor e   -> type_check_e e |> pt_if_rt is_intT intmsg
    | Not e    -> type_check_e e |> pt_if_rt is_BoolT "BoolT"
    (* binary expression *)
    | Or (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_BoolT "BoolT")
    | And (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_BoolT "BoolT")
    | Eq (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_cmpT cmpmsg) |> pt_if_never_slice |> (fun x -> T.NamedT "bool")
    | Neq (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_cmpT cmpmsg) |> pt_if_never_slice |> (fun x -> T.NamedT "bool")
    | Gt (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.NamedT "bool")
    | Gteq (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.NamedT "bool")
    | Lt (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.NamedT "bool")
    | Lteq (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.NamedT "bool")
    | Plus (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)
    | Minus (e1, e2) -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_numT nummsg)
    | Mult (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_numT nummsg)
    | Div (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_numT nummsg)
    | Mod (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Bor (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Band (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Xor (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Nand (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Lshft (e1, e2) -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    | Rshft (e1, e2) -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_intT intmsg)
    (* function calls *)
    | Append (e1, e2) ->
        let t = type_check_e e1 in
        (match rt t with
        | T.SliceT glt -> if glt = type_check_e e2 then t else raise (TypeCheckError "appended type != slice type")
        | _ -> raise (TypeCheckError "cannot append to non-slice"))
    | Cap e ->
        (match rt (type_check_e e) with
        | T.ArrayT (_,_) | T.SliceT _ -> T.NamedT "int"
        | _                           -> raise (TypeCheckError "cap only defined for array, slice"))
    | Len e ->
        (match rt (type_check_e e) with
        | T.ArrayT (_,_) | T.SliceT _ | T.StringT -> T.NamedT "int"
        | _                                       -> raise (TypeCheckError "len only defined for array, slice, string"))
    | FunctionCall ((Identifier str), es) ->
        let cat, glt = get_vcat_gltype_from_str str in
        (match cat with
        | T.Variable ->
            (match glt with
            | T.FunctionT (arg_glts, ret_glt) ->
                err_if (arg_glts <> List.map type_check_e es) (TypeCheckError "arg types given != expected"); ret_glt
            | _ -> raise (TypeCheckError (str^" is not a function")))
        | T.Type -> (* type cast *)
            let rt_caster, rt_castee = (rt glt), (rt (type_check_e (List.hd es))) in
            ignore (List.map (pt_if_rt is_basT basmsg) [rt_caster; rt_castee]);
            (match rt_caster, rt_castee with
            | T.StringT,_ when is_intT rt_castee                      -> T.NamedT str
            | _,        _ when is_numT rt_caster && is_numT rt_castee -> T.NamedT str
            | _,        _ when rt_caster = rt_castee                  -> T.NamedT str
            | _,_ -> raise (TypeCheckError "invalid type cast")))
    (* parentheses *)
    | ParenExpression e -> type_check_e e

and pt_if_type_check_eq e1 e2 =
    let t1, t2 = (type_check_e e1), (type_check_e e2) in
    if t1 = t2 then t1
    else
        let s1, s2 = (T.string_of_glt t1),(T.string_of_glt t2) in
        raise (TypeCheckError (s1 ^ " not equal to " ^ s2))

and type_check_idexp idexp =
    match idexp with
    | Ident str ->
        (let cat,glt = (get_vcat_gltype_from_str str) in
        (match glt with 
        | FunctionT (para, ret) -> raise ( TypeCheckError "Cannot index function calls")
        | _ -> (err_if (cat=T.Type) (TypeCheckError "")));
        glt)
    | Indexed (e1, e2) ->
        type_check_e e2 |> (pt_if_rt is_intT intmsg) |> (fun x -> ());
        (match rt (type_check_e e1) with
        | T.SliceT glt | T.ArrayT (_, glt) -> glt
        | _ -> raise (TypeCheckError ""))
    | StructAccess (e, str) ->
        (match (type_check_e e |> (pt_if_rt is_StructT sctmsg) |> rt) with
        | T.StructT stds ->
            let f acc (str',glt) =
                if acc <> None then acc else
                if str=str' then Some glt else None in
            let glto = List.fold_left f None stds in
            (match glto with
            | Some glt -> glt
            | None     -> raise (TypeCheckError ("struct does not have field "^str)))
        | _  -> raise (TypeCheckError "cannot access field of non-struct"))

and type_check_stmt s =
    match s with
    | ExpressionStatement e -> type_check_e e; T.Void
    | AssignmentStatement (es1, aop, es2) ->
        if (List.length es1)<>(List.length es2) then
        raise (TypeCheckError "multiple assignment size mismatch") else ();
        let f (e1,e2) =
            (match e1 with 
            | IdentifierExpression idexp ->
                (match idexp with
                | Ident str when str="_" -> type_check_e e2; ()
                | _ ->
                    if (type_check_e e1) <> (type_check_e e2) then
                    raise (TypeCheckError "assignment type mismatch") else ())
            | _ -> raise (TypeCheckError "Cannot assign to an expression anything")) in
        List.iter f (List.combine es1 es2);
        if (List.length es1 <> 1) && (aop <> ASG) then
        raise (TypeCheckError "cannot use shorthand operators in multiple assignment") else ();
        T.Void
    | ReturnStatement eo -> 
        (match eo with 
        | None -> T.Void
        | Some e -> type_check_e e)
    | ShortValDeclaration (ids, es) -> 
        if (List.length ids) = (List.length es) then ()
        else raise (TypeCheckError "multiple assignment size mismatch");
        if dup_exists ids then raise (TypeCheckError "duplicate ids") else ();
        List.iter (fun e -> type_check_e e; ()) es;
        let if_at_least_one = ref 0 in
        (let f (Identifier str as id, e) = 
            let id_type  = 
                let check cat glt = glt in
                    err_if_id_not_declared ~f:1 ~check:check id in
            match id_type with 
            | T.Void -> 
                if str = "_" then () else 
                (if_at_least_one := 1;
                let glt = type_check_e e in ();
                (match glt with 
                | T.Void -> raise (TypeCheckError "Void type cannot be assigned")
                | _ -> Hashtbl.add (context !current_scope) str (T.Variable, glt));); 
                ();
            | _ as glt ->
                if glt <> type_check_e e then 
                    raise (TypeCheckError "assignment type mismatch")
                else () in
        List.iter f (List.combine ids es) );
        if !if_at_least_one = 0 then 
            raise (TypeCheckError "At least one of the expressions on the LHS must be defined") else ();
        T.Void            
    | BlockStatements ss ->
        create_and_enter_child_scope ();
        List.iter (fun s -> type_check_stmt s; ())  ss;
        enter_parent_scope();
        T.Void
    | DeclarationStatement d -> type_check_decl d; T.Void
    | Inc e | Dec e -> type_check_e e |> (pt_if_rt is_numT nummsg); T.Void
    | PrintStatement (es) | PrintlnStatement (es) ->
        (match es with
        | None -> T.Void (*Do nothing *)
        | Some es ->
            let f e = ignore( type_check_e e |> (pt_if_rt (is_Ts (is_ordT :: [is_BoolT])) "Cannot print") ) in
            List.iter f es;
            T.Void);
        T.Void
    | Break | Continue | EmptyStatement -> T.Void (*Do nothing as trivial *)
    | ForStatement (s1, eo, s2, ss)  -> type_check_for (s1, eo, s2, ss)
    | IfStatement ifclause -> type_check_ifst ifclause 
    | SwitchStatement (s, eo, scl) -> type_check_switch (s, eo, scl); T.Void
    | _ -> raise (TypeCheckError "")

(*Subject to testing *)
and type_check_for f =
    match f with
    | (EmptyStatement, None, EmptyStatement, ss) ->
        create_and_enter_child_scope ();
        (*List.iter  (fun s -> type_check_stmt s; ()) ss;*)
        let tp = type_check_stmts ss in 
        enter_parent_scope();
        if !tp = !curr_ret_val then !tp else (tr_asn tp; T.Void)
    | (EmptyStatement, Some e, EmptyStatement, ss) ->
        type_check_e e |> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope();
        (*List.iter  (fun s -> type_check_stmt s; ()) ss;*)
        let tp = type_check_stmts ss in
        enter_parent_scope();
        if !tp = !curr_ret_val then T.Void else (tr_asn tp; T.Void)
    | (i, Some e, p , ss) ->
        create_and_enter_child_scope ();
        type_check_stmt i;
        type_check_e e |> pt_if_rt is_BoolT "BoolT";
        type_check_stmt p;
        create_and_enter_child_scope () ;
        (*List.iter  (fun s -> type_check_stmt s; ()) ss;*)
        let tp = type_check_stmts ss in 
        enter_parent_scope();
        enter_parent_scope();
        if !tp = !curr_ret_val then T.Void else (tr_asn tp; T.Void)
    (*TO ADD HERE ANOTHER CASE (stmt, None, None)*)
    | (i, None , EmptyStatement , ss) ->
        create_and_enter_child_scope();
        type_check_stmt i;
        create_and_enter_child_scope();
        let tp = type_check_stmts ss in 
        enter_parent_scope();
        enter_parent_scope();
        if !tp = !curr_ret_val then T.Void else (tr_asn tp; T.Void) 
    | _ -> raise (TypeCheckError "G")

and type_check_ifst ic =
    match ic with
    | If (s, e, ss, None) ->
        create_and_enter_child_scope();
        type_check_stmt s;
        type_check_e e|> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope ();
        let tp = type_check_stmts ss in 
        enter_parent_scope();
        enter_parent_scope();
        if !tp = !curr_ret_val then !tp else (tr_asn tp; T.Void)
        
    | If (s, e, ss, Some els) ->
        create_and_enter_child_scope();
        type_check_stmt s;
        type_check_e e|> pt_if_rt is_BoolT "BoolT";
        create_and_enter_child_scope();
        let tp = type_check_stmts ss in 
        enter_parent_scope();
        if !tp = !curr_ret_val then !tp else (tr_asn tp; T.Void);
        (*print_int !tag_ret; T.Void;*)
        let ret_val =
            (match els with
            | Elseif ifs -> type_check_ifst ifs;
            | Else ss->
                create_and_enter_child_scope();
                let tp1 = type_check_stmts ss in 
                enter_parent_scope();      
                if !tp1 = !curr_ret_val then !tp1 else (tr_asn tp; T.Void)) in
        enter_parent_scope();
        ret_val
       
            

and type_check_switch sw =
    match sw with
    | (s, None , swcl ) ->
        create_and_enter_child_scope();
        type_check_stmt s;
        List.iter (
            fun swc -> match swc with
            | Default ss ->
                create_and_enter_child_scope();
                let tp = type_check_stmts ss in 
                enter_parent_scope();
                if !tp = !curr_ret_val then (temp_ret := !tp; T.Void) else (tr_asn tp; T.Void); () 
            | Case (el, ss) ->
                List.iter (fun e -> type_check_e e |> pt_if_rt is_BoolT "BoolT"; ()) el;
                create_and_enter_child_scope();
                let tp = type_check_stmts ss in 
                enter_parent_scope();
                if !tp = !curr_ret_val then (temp_ret := !tp; T.Void) else (tr_asn tp; T.Void);
                ()) swcl;
                enter_parent_scope();
                T.Void
    | (s, Some e, swcl) ->
        create_and_enter_child_scope();
        type_check_stmt s;
        type_check_e e |> pt_if_rt is_cmpT cmpmsg;
        List.iter (fun swc -> match swc with
            | Default ss ->
                create_and_enter_child_scope();
                let tp = type_check_stmts ss in 
                enter_parent_scope();
                if !tp = !curr_ret_val then (temp_ret := !tp; T.Void) else (tr_asn tp; T.Void); ()
            | Case (el, ss) ->
                List.iter (fun e2 -> pt_if_type_check_eq e e2; ()) el;
                create_and_enter_child_scope();
                let tp = type_check_stmts ss in 
                enter_parent_scope();
                if !tp = !curr_ret_val then (temp_ret := !tp; T.Void) else (tr_asn tp; T.Void);
                
                ()) swcl;
            enter_parent_scope();
            T.Void

and type_check_stmts ss =
    let last_i = (List.length ss)-1 in
    let f (i,glt) s =
        let next_glt = type_check_stmt s in
        if next_glt <> T.Void && i <> last_i then
            raise (TypeCheckError "dead code, return statement not last")
        else (i+1,next_glt) in
    ref (snd (List.fold_left f (0,T.Void) ss))

and tr_asn t =
    match !t with 
    | T.Void -> if (!tag_ret = 0) then tag_ret := 1 else if (!tag_ret = 2) then tag_ret := 3 else ();
    | _ -> if(!tag_ret = 0) then tag_ret := 2  else if (!tag_ret = 1) then tag_ret := 3 else ();
    
