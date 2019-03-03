let print_program prog =
    print_endline "ok let's start this shit"

    #use "tree.ml"

let rec pretty_print inp = 
  match inp with 
  | Or(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | And(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Eq(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Neq(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Gt(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Gteq(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Lt(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Lteq(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Plus(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Minus(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Bor(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Xor(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Mult(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Div(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Mod(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Lshft(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Rshft(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Band(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Nand(id1, id2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Uplus(id) -> print_endline ("||") pretty_print id 
  | Uminus(id) -> print_endline ("||") pretty_print id 
  | Not(id) -> print_endline ("||") pretty_print id
  | Uxor(id) -> print_endline ("||") pretty_print id
  | FunctionCall(s, e) -> pretty_print s; print_string ("||"); pretty_print e 
  | Append(e1, e2) -> pretty_print id1; print_string ("||"); pretty_print id2
  | Len (e) -> print_endline("len")
  | Cap (e) -> print_endline ("Cap")
  | ParenExpression (e) -> print_endline ("e")
  | LitInt (i) -> print_endline "LitInt"
  | LitFloat (i) -> print_endline "LitInt"
  | LitBool (i) -> print_endline "LitInt"
  | LitRune (i) -> print_endline "LitInt"
  | LitString (i) -> print_endline "LitInt"
  | IdentifierExpression (ident) -> print_endline "LitInt"
  | _   -> print_endline ("end")