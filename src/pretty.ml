

#use "tree.ml"

let rec pretty_exp inp = 
  match inp with 
  | Or(id1, id2) -> print_string "("; pretty_exp id1; print_string ("||"); pretty_exp id2; print_string ")"
  | And(id1, id2) ->print_string "("; pretty_exp id1; print_string ("&&"); pretty_exp id2; print_string ")"
  | Eq(id1, id2) -> print_string "("; pretty_exp id1; print_string ("=="); pretty_exp id2; print_string ")"
  | Neq(id1, id2) -> print_string "("; pretty_exp id1; print_string ("!="); pretty_exp id2; print_string ")"
  | Gt(id1, id2) -> print_string "("; pretty_exp id1; print_string (">"); pretty_exp id2; print_string ")"
  | Gteq(id1, id2) -> print_string "("; pretty_exp id1; print_string (">="); pretty_exp id2; print_string ")"
  | Lt(id1, id2) -> print_string "("; pretty_exp id1; print_string ("<"); pretty_exp id2; print_string ")"
  | Lteq(id1, id2) -> print_string "(";  pretty_exp id1; print_string ("<="); pretty_exp id2; print_string ")"
  | Plus(id1, id2) -> print_string "("; pretty_exp id1; print_string ("+"); pretty_exp id2; print_string ")"
  | Minus(id1, id2) -> print_string "("; pretty_exp id1; print_string ("-"); pretty_exp id2; print_string ")"
  | Bor(id1, id2) -> print_string "("; pretty_exp id1; print_string ("|"); pretty_exp id2; print_string ")"
  | Xor(id1, id2) -> print_string "("; pretty_exp id1; print_string ("^"); pretty_exp id2; print_string ")"
  | Mult(id1, id2) -> print_string "("; pretty_exp id1; print_string ("*"); pretty_exp id2; print_string ")"
  | Div(id1, id2) -> print_string "("; pretty_exp id1; print_string ("/"); pretty_exp id2; print_string ")"
  | Mod(id1, id2) -> print_string "("; pretty_exp id1; print_string ("%"); pretty_exp id2; print_string ")"
  | Lshft(id1, id2) -> print_string "("; pretty_exp id1; print_string ("<<"); pretty_exp id2; print_string ")"
  | Rshft(id1, id2) -> print_string "("; pretty_exp id1; print_string (">>"); pretty_exp id2;print_string ")"
  | Band(id1, id2) -> print_string "("; pretty_exp id1; print_string ("&"); pretty_exp id2; print_string ")"
  | Nand(id1, id2) -> print_string "("; pretty_exp id1; print_string ("nand"); pretty_exp id2; print_string ")"
  | Uplus(id) -> print_string "("; print_endline ("+"); pretty_exp id ; print_string ")"
  | Uminus(id) -> print_string "("; print_endline ("-"); pretty_exp id ; print_string ")"
  | Not(id) -> print_string "("; print_endline ("!"); pretty_exp id; print_string ")"
  | Uxor(id) -> print_string "("; print_endline ("^"); pretty_exp id; print_string ")"
  | FunctionCall(s, e) ->print_string "(";  print_string s; List.iter (pretty_exp) e; print_string ")"
  | Append(e1, e2) -> print_string "("; print_string "Append ("; pretty_exp e1;  pretty_exp e2; print_string ")";print_string ")" 
  | Len (e) -> print_string "("; print_string("Len ("); pretty_exp e; print_string ")"; print_string ")"
  | Cap (e) -> print_string "("; print_endline ("Cap ("); pretty_exp e; print_string ")"; print_string ")"
  | ParenExpression (e) -> print_string "("; List.iter (pretty_exp) [e]; print_string ")"
  | LitInt (i) -> print_string "("; print_int i; print_string ")"
  | LitFloat (i) -> print_string "("; print_float i;print_string ")" 
  | LitBool (i) -> print_string "("; print_string "LitInt"; print_string ")"
  | LitRune (i) -> print_string "("; print_string i; print_string ")"
  | LitString (i) -> print_string "("; print_string i; print_string ")"
  | IdentifierExpression (ident) -> print_string "("; pretty_ident ident; print_endline "LitInt"; print_string ")" 
  | _   -> print_endline ("end")
and pretty_ident inp =
  match inp with 
  | Ident (s) -> print_string s 
  | Blankid -> print_string "_"
  | Indexed (s, e) -> print_string s; pretty_exp e 
  | StructAccess (s, id) -> print_string s; pretty_ident id
  | _ -> print_endline "end"