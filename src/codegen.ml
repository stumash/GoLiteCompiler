open Tree
open Typecheck
open Helpers
open Cactus_stack
open Golitetypes

let p s = print_string s;

(* print_indent indent_level *)
let p_ind il = p (String.make (il * 2) ' ')

let z (ln,cn) =
    if ln,cn = -2,-2 then "" else
    let f i = if i < 0 then "_"^(string_of_int (-1 * i)) else string_of_int i
    "_"^(f ln)^"_"^(f cn)

let current_scope = ref global_scope

let rec cg_program ast =
    match ast with
    | Program (p, dl) ->
        p "#include <stdio.h>\n";
        p "#include <string.h>\n";
        p "#include <stdlib.h>\n";
        p "#include <stdbool.h>\n"; (* TODO: collect all struct type literals, prepend typedefs *)
        p "\n";
        List.iter cg_decl
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
                List.map (fun (Identifier (str, pos)) -> snd3 (lookup ~current_scope str pos)) ids
            let f (id, (glt, eo)) =
                cg_glt_id ~il glt id
                (match eo with
                | Some e -> p " = "; cg_exp e
                | None   -> ());
                p ";\n" in
            List.iter f (List.combine ids, List.comgine glts eos) in
        List.iter cg_vd vds
    | TypeDeclaration (tds, _) -> () (* TODO *)

and cg_id ?(il=0) (Identifier (str, pos)) =
    if str = "_" then p ("_"^(z pos)) else
    let pos = trd3 (lookup ~current_scope str pos) in
    p (str^(z pos))

(* print the type AND the associated identifier (for type or variable declarations) *)
and cg_glt_id ?(il=0) glt id =
    match glt with
    | NamedT (str, pos) -> p_ind il; p str; p (z pos); p " "; cg_id id
    | StructT stds ->
        let cg_std ?(il=0) (str,glt) =
            p_ind il; cg_glt_id glt (Identifier (str, (-2,-2))); p ";\n" in
        p "struct{\n";
        List.iter (cd_std ~il:(il+1)) stds
        p"}\n"
    | ArrayT (i, glt) ->
        cg_glt_id glt id; 

and cg_exp ?(il=0) e = () (* TODO *)
