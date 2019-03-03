

#use "tree.ml"

let rec pretty_exp inp = 
  match inp with 
  | Or(id1, id2) -> pretty_exp id1; print_string ("||"); pretty_exp id2
  | And(id1, id2) -> pretty_exp id1; print_string ("&&"); pretty_exp id2
  | Eq(id1, id2) -> pretty_exp id1; print_string ("=="); pretty_exp id2
  | Neq(id1, id2) -> pretty_exp id1; print_string ("!="); pretty_exp id2
  | Gt(id1, id2) -> pretty_exp id1; print_string (">"); pretty_exp id2
  | Gteq(id1, id2) -> pretty_exp id1; print_string (">="); pretty_exp id2
  | Lt(id1, id2) -> pretty_exp id1; print_string ("<"); pretty_exp id2
  | Lteq(id1, id2) -> pretty_exp id1; print_string ("<="); pretty_exp id2
  | Plus(id1, id2) -> pretty_exp id1; print_string ("+"); pretty_exp id2
  | Minus(id1, id2) -> pretty_exp id1; print_string ("-"); pretty_exp id2
  | Bor(id1, id2) -> pretty_exp id1; print_string ("|"); pretty_exp id2
  | Xor(id1, id2) -> pretty_exp id1; print_string ("^"); pretty_exp id2
  | Mult(id1, id2) -> pretty_exp id1; print_string ("*"); pretty_exp id2
  | Div(id1, id2) -> pretty_exp id1; print_string ("/"); pretty_exp id2
  | Mod(id1, id2) -> pretty_exp id1; print_string ("%"); pretty_exp id2
  | Lshft(id1, id2) -> pretty_exp id1; print_string ("<<"); pretty_exp id2
  | Rshft(id1, id2) -> pretty_exp id1; print_string (">>"); pretty_exp id2
  | Band(id1, id2) -> pretty_exp id1; print_string ("&"); pretty_exp id2
  | Nand(id1, id2) -> pretty_exp id1; print_string ("nand"); pretty_exp id2
  | Uplus(id) -> print_endline ("+"); pretty_exp id 
  | Uminus(id) -> print_endline ("-"); pretty_exp id 
  | Not(id) -> print_endline ("!"); pretty_exp id
  | Uxor(id) -> print_endline ("^"); pretty_exp id
  | FunctionCall(s, e) -> print_string s; List.iter (pretty_exp) e 
  | Append(e1, e2) -> print_string "Append ("; pretty_exp e1;  pretty_exp e2; print_string ")"
  | Len (e) -> print_string("Len ("); pretty_exp e; print_string ")"
  | Cap (e) -> print_endline ("Cap ("); pretty_exp e; print_string ")"
  | ParenExpression (e) -> print_string "("; List.iter (pretty_exp) [e]; print_string ")"
  | LitInt (i) -> print_int i
  | LitFloat (i) -> print_float i
  | LitBool (i) -> print_string "LitInt"
  | LitRune (i) -> print_string i
  | LitString (i) -> print_string i
  | IdentifierExpression (ident) -> pretty_ident ident; print_endline "LitInt"
  | _   -> print_endline ("end")
and pretty_ident inp =
  match inp with 
  | Ident (s) -> print_string s 
  | Blankid -> print_string "_"
  | Indexed (s, e) -> print_string s; pretty_exp e 
  | StructAccess (s, id) -> print_string s; pretty_ident id
  | _ -> print_endline "end"