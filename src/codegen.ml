open Tree
open Typecheck
open Helpers

let p s = print_string s;

let rec cg_program ast = 
  match ast with 
  | Program (p, dl) -> List.iter cg_decl
  | EmptyProgram -> () 
  
and cg_decl dec = 
  match dec with 
| FunctionDeclaration (id, para, tso, ss, pos) -> ()
  (*cg_ident id; cg_para para; List.iter cg_stmt; cg_type tso*)
| VariableDeclaration (vds, pos) ->
  let cg_vd (ids, tpo, eso) =
    List.iter (fun (id, exp) -> 
                (match tpo with 
                | Some tpo -> cg_type tpo
                | None     -> () (*Fetch from Symbol table*) );
                cg_ident id;
                (match exp with 
                | Some exp -> p "="; cg_exp exp; p ";"; ()
                | None     -> p ";\n"; () (*Do nothign*)) ;
              ()) (List.combine ids, Some eso); ()
| TypeDeclaration (tds, _) -> ();
  (*Take care of this later *)
  
and cg_ident (Identifier (str, pos)) =
  (*If it is a blank identifier, just print the '_' with its corresponding line number *)
  (*Fetch str from symbol table *)
  (*Print its name concatenated with it's line_number mentioned in the symbol table*)

and cg_type tp = 
  match tp with 
  | IdentifierType id -> pp_id id
  | ArrayTypeLiteral (e, tp, _) -> (*Fethc type from symbol table *)


  

    