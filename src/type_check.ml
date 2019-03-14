open Tree
open Helpers
module CS = Cactus_stack
module T = Golitetypes

let global_context =
    let ht : (string, (T.vcat * T.gltype)) Hashtbl.t = Hashtbl.create 128 in
    (* builtin types *)
    Hashtbl.add ht "int"     (T.Type, T.IntT);
    Hashtbl.add ht "float64" (T.Type, T.FloatT);
    Hashtbl.add ht "bool"    (T.Type, T.BoolT);
    Hashtbl.add ht "rune"    (T.Type, T.RuneT);
    Hashtbl.add ht "string"  (T.Type, T.StringT);
    (* literals *)
    Hashtbl.add ht "true"  (T.Constant, T.NamedType "bool");
    Hashtbl.add ht "false" (T.Constant, T.NamedType "bool");
    Hashtbl.add ht  x;
    ht

let global = CS.CsNode {
    parent = CsRoot
    children = []
    context = global_context
}

let current_scope = ref global

let err_if_id_in_current_scope (Identifier str) =
    try
        let _ = Hashtbl.find (!current_scope).context str in ()
    with
    | Hashtbl.Not_found -> raise TypeCheckError (str ^ " is already in")

let err_if_id_not_in_scope (Identifer str) =
    let err_if_id_not_in_scope' str scope =
        try
            let _ = Hashtbl.find (!scope).context str in ()
        with
        | Hashtbl.Not_found ->
            match (!current_scope).parent with
            | CsRoot -> raise TypeCheckError (str ^ " not found in scope")
            | csn    -> err_if_id_not_in_scope' str csn.parent
   in
   err_if_id_not_in_scope' str current_scope

let type_check_prog prog =
    | EmptyProgram -> true
    | Program (pkg, ds) -> List.iter type_check_decl ds

let type_check_decl d =
    match d with
    | VariableDeclaration vds -> List.iter type_check_vd vds
    | TypeDeclaration tds -> List.iter type_check_td tds
    | FunctionDeclaration (id, prmrs, tso, ss) ->

let type_check_ts ts =
    | Identifier str ->

let type_check_vd (ids, tso, eso) =
    List.iter err_if_id_in_current_scope ids;

let type_check_td (id, ts) =
    match id, ts with
    |
