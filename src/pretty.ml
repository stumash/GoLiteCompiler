open Tree
open Printf

(* pp - Pretty Print *)
let rec pp_prog prog =
    match prog with
    | _ -> p "let's get this shit started!"

and pp_exp exp =
    match exp with
    | Or (e1, e2)                   -> pp_exp e1; p " || "; pp_exp e2
    | And (e1, e2)                  -> pp_exp e1; p " && "; pp_exp e2
    | Eq (e1, e2)                   -> pp_exp e1; p " == "; pp_exp e2
    | Neq (e1, e2)                  -> pp_exp e1; p " != "; pp_exp e2
    | Gt (e1, e2)                   -> pp_exp e1; p " > "; pp_exp e2
    | Gteq (e1, e2)                 -> pp_exp e1; p " >= "; pp_exp e2
    | Lt (e1, e2)                   -> pp_exp e1; p " < "; pp_exp e2
    | Lteq (e1, e2)                 -> pp_exp e1; p " <= "; pp_exp e2
    | Plus (e1, e2)                 -> pp_exp e1; p " + "; pp_exp e2
    | Minus (e1, e2)                -> pp_exp e1; p " - "; pp_exp e2
    | Bor (e1, e2)                  -> pp_exp e1; p " | "; pp_exp e2
    | Xor (e1, e2)                  -> pp_exp e1; p " ^ "; pp_exp e2
    | Mult (e1, e2)                 -> pp_exp e1; p " * "; pp_exp e2
    | Div (e1, e2)                  -> pp_exp e1; p " / "; pp_exp e2
    | Mod (e1, e2)                  -> pp_exp e1; p " % "; pp_exp e2
    | Lshft (e1, e2)                -> pp_exp e1; p " << "; pp_exp e2
    | Rshft (e1, e2)                -> pp_exp e1; p " >> "; pp_exp e2
    | Band (e1, e2)                 -> pp_exp e1; p " & "; pp_exp e2
    | Nand (e1, e2)                 -> pp_exp e1; p " &^ "; pp_exp e2
    | Uplus (e)                     -> p " + "; pp_exp e
    | Uminus (e)                    -> p " - "; pp_exp e
    | Not (e)                       -> p " ! "; pp_exp e
    | Uxor (e)                      -> p " ^ "; pp_exp e
    | FunctionCall (s, es)          -> printf "%s(" s; pp_explist es; p ")"
    | Append (e1, e2)               -> p "append("; pp_explist [e1;e2]; p ")"
    | Len (e)                       -> p "len("; pp_exp e; p ")"
    | Cap (e)                       -> p "cap("; pp_exp e; p ")"
    | ParenExpression (e)           -> p "("; pp_exp e; p ")"
    | LitInt (i)                    -> print_int i
    | LitFloat (f)                  -> print_float f
    | LitBool (b)                   -> printf "%B" b; ()
    | LitRune (s)                   -> p s
    | LitString (s)                 -> p s
    | IdentifierExpression (id_exp) -> pp_id_exp id_exp

and pp_id_exp id_exp =
    match id_exp with
    | Ident (s)                -> p s
    | Blankid                  -> p "_"
    | Indexed (s, e)           -> printf "%s[" s; pp_exp e; p "]"
    | StructAccess (s, id_exp) -> printf "%s." s; pp_id_exp id_exp

(* helpers *)
and p = print_string
and pp_explist es =
    let f = (fun i e -> pp_exp e; if i != (List.length es)-1 then p ", " else ()) in
    List.iteri f es

