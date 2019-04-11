open Tree
open Typecheck
open Helpers
open Cactus_stack
open Golitetypes

(* append positions to variable names for overshadowing *)
(* for every non-declared variable in current scope, use 'nonlocal' on it *)

let p s = print_string s

let p_ind il = p (String.make (il * 4) ' ')

let z (ln,cn) =
    if (ln,cn) = (-2,-2) then "" else
    let f i = if i < 0 then "_"^(string_of_int (-1 * i)) else string_of_int i in
    "_"^(f ln)^"_"^(f cn)

let rec cg_glt_default str glt =
    match glt with
    | Void | FunctionT (_,_) -> raise (CodegenError "HOW YOU DO THIS TO ME?!")
    | IntT                   -> str^" = 0"
    | FloatT                 -> str^" = 0.0"
    | BoolT                  -> str^" = False"
    | RuneT                  -> str^" = 0"
    | StringT                -> str^" = \"\""
    | SliceT                 -> str^" = go_slice()"
    | NamedT (str, pos)      -> cg_glt_default (rt glt)
    | ArrayT (i,_)           -> ()
    | StructT stds           -> ()


let current_scope = ref global_scope

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
            let glts =
                List.map (fun (Identifier (str, pos)) -> snd3 (lookup ~current_scope str pos)) ids in
            let f (id, (glt, eo)) =
                cg_id ~il id
                (match eo with
                | Some e -> p " = "; cg_exp e
                | None   -> ());
                p ";\n" in
            List.iter f (List.combine ids (List.combine glts eos)) in
        List.iter cg_vd vds
    | TypeDeclaration (tds, _) -> ()

and cg_id ?(il=0) (Identifier (str, pos)) =
    if str = "_" then p ("_"^(z pos)) else
    let pos = trd3 (lookup ~current_scope str pos) in
    p (str^(z pos))

and cg_exp ?(il=0) e = () (* TODO *)
