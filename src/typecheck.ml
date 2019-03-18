open Tree
open Helpers
open Cactus_stack
module T = Golitetypes

(* DATA STRUCTURES ------------------------------------------------------------------------------------ *)

let root_context =
    let ht : (string, (T.vcat * T.gltype)) Hashtbl.t = Hashtbl.create 8 in
    (* builtin types *)
    Hashtbl.add ht "int"     (T.Type, T.IntT);
    Hashtbl.add ht "float64" (T.Type, T.FloatT);
    Hashtbl.add ht "bool"    (T.Type, T.BoolT);
    Hashtbl.add ht "rune"    (T.Type, T.RuneT);
    Hashtbl.add ht "string"  (T.Type, T.StringT);
    (* literals *)
    Hashtbl.add ht "true"  (T.Constant, T.NamedT "bool");
    Hashtbl.add ht "false" (T.Constant, T.NamedT "bool");
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
    | CsRoot -> raise (TypeCheckError "IMPOSSIBLE")
    | CsNode { parent; children; context } ->
        children := global_scope :: !children

(* initialized to global scope, the only child of the root scope *)
let current_scope = ref global_scope

(* HELPERS -------------------------------------------------------------------------------------------- *)

let zip l1 l2 = List.map2 (fun a b -> a,b) l1 l2
let unzip xys = List.fold_right (fun (x,y) (acc1,acc2) -> (x::acc1,y::acc2)) xys ([],[])

let is_IntT t    = match t with | T.IntT -> true | _ -> false
let is_FloatT t  = match t with | T.FloatT -> true | _ -> false
let is_BoolT t   = match t with | T.BoolT -> true | _ -> false
let is_RuneT t   = match t with | T.RuneT -> true | _ -> false
let is_StringT t = match t with | T.StringT -> true | _ -> false
let is_StructT t = match t with | T.StructT _ -> true | _ -> false
let is_SliceT t  = match t with | T.SliceT _ -> true | _ -> false
let is_ArrayT t  = match t with | T.ArrayT (_,_) -> true | _ -> false
let is_FunctionT t = match t with | T.FunctionT (_,_) -> true | _ -> false

let is_intT = [is_IntT; is_RuneT]
let is_numT = is_intT @ [is_FloatT]
let is_ordT = is_numT @ [is_StringT]
let is_cmpT = is_ordT @ [is_BoolT; is_StructT; is_ArrayT]
let intmsg  = "IntT, RuneT"
let nummsg  = "IntT, RuneT, FloatT"
let ordmsg  = "IntT, RuneT, FloatT, StringT"
let cmpmsg  = "IntT, RuneT, FloatT, StringT, BoolT, StructT, ArrayT"
let sctmsg  = "StructT"

let get_vcat_gltype_from_str str =
    let rec get_vcat_gltype_from_str' str scope =
        try Hashtbl.find (context scope) str with
        | Not_found ->
            (match parent scope with
            | CsRoot -> raise (TypeCheckError (str ^ " not declared"))
            | csn    -> get_vcat_gltype_from_str' str csn) in
    get_vcat_gltype_from_str' str !current_scope

let rt glt =
    let rec rt' glt scope =
        match glt with
        | T.NamedT str -> 
            (try rt' (snd (Hashtbl.find (context scope) str)) scope with
            | Not_found ->
                (match parent scope with
                | CsRoot -> raise (TypeCheckError (str^" not declared"))
                | csn    -> rt' glt csn))
        | _ -> glt in
    rt' glt !current_scope

(* Pass-Through [y] If Resolved Type [of y is] In [list xs] *)
let pt_if_rt fs msg y =
    let y' = rt y in
    if List.exists (fun f -> f y') fs then y
    else raise (TypeCheckError ("RT( "^(T.string_of_glt y)^" ) not in "^msg))

let err_if_id_in_current_scope (Identifier str) =
    try
        let _ = Hashtbl.find (context !current_scope) str in
        raise (TypeCheckError (str ^ " is already in current scope"))
    with
    | Not_found -> ()

(* recurse until entry found, then do check *)
let err_if_id_not_declared ?(check=(fun a b -> ())) (Identifier str) =
    let rec err_if_id_not_declared' str scope =
        try
            let (cat, glt) = Hashtbl.find (context scope) str in
            check cat glt
        with
        | Not_found ->
            (match parent scope with
            | CsRoot -> raise (TypeCheckError (str ^ " not declared"))
            | csn    -> err_if_id_not_declared' str csn)
   in
   err_if_id_not_declared' str !current_scope

let err_if_type_not_declared (IdentifierType ((Identifier str) as id)) =
    let check cat glt =
        if cat = T.Type then ()
        else
            let scat = T.string_of_vcat cat in
            let s = (str ^ " is a " ^ scat ^ ", not a Type") in
            raise (TypeCheckError s)
    in
    err_if_id_not_declared ~check id

let create_new_scope () = 
    let add new_scope = 
        match !current_scope with 
        | CsRoot -> raise (TypeCheckError "IMPOSSIBLE")
        | CsNode {parent; children; context} -> 
            children := new_scope :: !children in
    let new_scope = CsNode { parent=(!current_scope); children=ref []; context=(Hashtbl.create 8)} in 
    add new_scope; current_scope := new_scope

let get_parent_scope () = 
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
    | VariableDeclaration vds                  -> List.iter type_check_vd vds
    | TypeDeclaration tds                      -> List.iter type_check_td tds
    | FunctionDeclaration (id, prmrs, tso, ss) -> () (* TODO *)

and type_check_vd (ids, tso, eso) =
    List.iter err_if_id_in_current_scope ids;
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
        Hashtbl.add (context !current_scope) str (T.Variable, glt) in
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
        let type_check_std (ids, ts) = 
            let strs = List.map (fun (Identifier str) -> str) ids in
            (strs, (type_check_ts ts)) in
        StructT (List.map type_check_std stds)

and type_check_td (((Identifier str) as id), ts) =
    err_if_id_in_current_scope id;
    let glt = type_check_ts ts in
    Hashtbl.add (context !current_scope) str (T.Type, glt)

and type_check_fd (Identifier str as id, Parameters prms, tso, ss) =
    err_if_id_in_current_scope id;
    let idss, tss = unzip prms in
    let prm_types = List.map type_check_ts tss in
    let ret_type = match tso with | None -> T.Void | Some ts -> type_check_ts ts in
    Hashtbl.add (context !current_scope) str (T.Variable, T.FunctionT (prm_types, ret_type));
    create_new_scope ();
    let add_ids_to_scope (ids, glt) =
        List.iter (fun (Identifier str) -> Hashtbl.add (context !current_scope) str (T.Variable, glt)) ids in
    List.iter add_ids_to_scope (zip idss prm_types);
    List.iter (fun s -> type_check_stmt s; ()) ss
    (* TODO extract return type, should make function for statement list *)

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
    | Not e    -> type_check_e e |> pt_if_rt [is_BoolT] "BoolT"
    (* binary expression *)
    | Or (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt [is_BoolT] "BoolT")
    | And (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt [is_BoolT] "BoolT")
    | Eq (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_cmpT cmpmsg)   |> (fun x -> T.BoolT)
    | Neq (e1, e2)   -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_cmpT cmpmsg)   |> (fun x -> T.BoolT)
    | Gt (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.BoolT)
    | Gteq (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.BoolT)
    | Lt (e1, e2)    -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.BoolT)
    | Lteq (e1, e2)  -> pt_if_type_check_eq e1 e2 |> (pt_if_rt is_ordT ordmsg)   |> (fun x -> T.BoolT)
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
        (match type_check_e e1 with
        | T.SliceT glt as t -> if glt = type_check_e e2 then t else raise (TypeCheckError "")
        | _ -> raise (TypeCheckError ""))
    | Cap e ->
        (match type_check_e e with
        | T.ArrayT (_,_) | T.SliceT _ -> T.IntT
        | _                           -> raise (TypeCheckError ""))
    | Len e ->
        (match type_check_e e with
        | T.ArrayT (_,_) | T.SliceT _ | T.StringT -> T.IntT
        | _                                       -> raise (TypeCheckError ""))
    | FunctionCall ((Identifier str ), es) ->
        (* TODO: handle casting!!! *)
        let cat, glt = get_vcat_gltype_from_str str in
        (err_if (cat != T.Variable) (TypeCheckError (str^" is not a function"));
        (match glt with
        | T.FunctionT (arg_glts, ret_glt) ->
            err_if (arg_glts != List.map type_check_e es) (TypeCheckError ""); ret_glt
        | _ -> raise (TypeCheckError (str^" is not a function"))))
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
        let cat,glt = (get_vcat_gltype_from_str str) in
        (err_if (cat=T.Type) (TypeCheckError "");
        glt)
    | Indexed (e1, e2) ->
        type_check_e e2 |> (pt_if_rt is_intT intmsg) |> (fun x -> ());
        (match rt (type_check_e e1) with
        | T.SliceT glt | T.ArrayT (_, glt) -> glt
        | _ -> raise (TypeCheckError ""))
    | StructAccess (e, str) ->
        (match (type_check_e e |> (pt_if_rt [is_StructT] sctmsg) |> rt) with
        | T.StructT stds ->
            let glto =
                let f (strs, glt) = if (List.exists ((=) str) strs) then Some glt else None in
                List.fold_left (fun acc tup -> if acc != None then acc else f tup) None stds in
            (match glto with
            | Some glt -> glt
            | None     -> raise (TypeCheckError ""))
        | _  -> raise (TypeCheckError ""))

and type_check_stmt s = 
    match s with 
    | ExpressionStatement e -> type_check_e e; T.Void 
(*    | ReturnStatement e -> ( ) (*To be combined with functions so wait*)
    | ShortValDeclaration (ids, es) -> () (*TO DO*) *)
    | BlockStatements ss -> 
        create_new_scope ();
        List.iter (fun s -> type_check_stmt s; ())  ss; 
        get_parent_scope();
        T.Void 
    | DeclarationStatement d -> type_check_decl d; T.Void
    | Inc e | Dec e -> type_check_e e |> (pt_if_rt is_numT nummsg); T.Void 
    | PrintStatement (es) | PrintlnStatement (es) ->
        (match es with 
        | None -> T.Void (*Do nothing *)
        | Some es -> List.iter (fun e ->  type_check_e e; ()) es; T.Void 
        ); T.Void           
    | Break | Continue | EmptyStatement -> T.Void (*Do nothing as trivial *)
    | ForStatement (s1, eo, s2, ss)  -> type_check_for (s1, eo, s2, ss); T.Void
    | IfStatement ifclause -> type_check_ifst ifclause ; T.Void 
    | SwitchStatement (s, eo, scl) -> (*type_check_switch (s, eo, scl);*) T.Void 
    | _ -> raise (TypeCheckError "")

(*Subject to testing *)
and type_check_for f = 
    match f with 
    | (EmptyStatement, None, EmptyStatement, ss) -> 
        create_new_scope ();
        List.iter (fun s -> type_check_stmt s; ()) ss;
        get_parent_scope()
    | (EmptyStatement, Some e, EmptyStatement, ss) -> 
        type_check_e e |> pt_if_rt [is_BoolT] "Bool";  
        create_new_scope();
        List.iter (fun s -> type_check_stmt s; ()) ss;
        get_parent_scope()
    | (i, Some e, p , ss) -> 
        create_new_scope ();
        type_check_stmt i;
        type_check_e e |> pt_if_rt [is_BoolT] "Bool"; 
        type_check_stmt p; 
        create_new_scope () ;
        List.iter  (fun s -> type_check_stmt s; ()) ss;
        get_parent_scope();
        get_parent_scope()
    | _ -> raise (TypeCheckError "")

and type_check_ifst ic = 
    match ic with 
    | If (s, e, ss, None) -> 
        create_new_scope();
        type_check_stmt s;
        type_check_e e|> pt_if_rt [is_BoolT] "Bool";
        create_new_scope ();
        List.iter (fun s -> type_check_stmt s; () ) ss;
        get_parent_scope();
        get_parent_scope();
        T.Void            
    | If (s, e, ss, Some els) ->
        create_new_scope();
        type_check_stmt s;
        type_check_e e|> pt_if_rt [is_BoolT] "Bool";
        create_new_scope();
        List.iter (fun s -> type_check_stmt s; () ) ss;
        get_parent_scope();
        get_parent_scope();
        (match els with 
        | Elseif ifs -> type_check_ifst ifs; T.Void 
        | Else ss-> 
            create_new_scope();
            List.iter (fun s -> type_check_stmt s; () ) ss; 
            get_parent_scope();
            T.Void );
            T.Void

and type_check_switch sw = 
    match sw with 
    | (s, None , swcl ) -> 
        create_new_scope();
        type_check_stmt s;
        (match swcl with 
        | Default ss -> 
            create_new_scope();
            List.iter (fun s -> type_check_stmt s; () ) ss; 
            get_parent_scope();
            T.Void;
        | Case (el, ss) -> 
            List.iter (fun e -> type_check_e e |> pt_if_rt [is_BoolT] "Bool"; ()) el;
            create_new_scope();   
            List.iter (fun s -> type_check_stmt s; () ) ss; 
            get_parent_scope();
            T.Void);
            get_parent_scope();
            T.Void
    | (s, Some e, swcl) ->
        create_new_scope();
        type_check_stmt s;
        type_check_e e |> pt_if_rt is_cmpT cmpmsg;
        (match swcl with 
        | Default ss -> 
            create_new_scope();
            List.iter (fun s -> type_check_stmt s; () ) ss; 
            get_parent_scope();
            T.Void;
        | Case (el, ss) -> 
            List.iter (fun e2 -> pt_if_type_check_eq e e2; ()) el;
            create_new_scope();
            List.iter (fun s -> type_check_stmt s; () ) ss; 
            get_parent_scope();
            T.Void);
            get_parent_scope(); 
            T.Void
