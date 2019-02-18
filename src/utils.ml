(* TODO:
 *
 * The OCaml standard library is notoriously bad, as far as standard libraries go.
 *
 * I would prefer to use an alternate standard library called 'Core', which is
 * very popular.
 *
 * We will need to ask Alex to install 'opam', the OCaml package manager, on the
 * teaching.cs.mcgill.ca machines. Then, we will install Core and corebuild.
 *
 * *)

(* go-style int string to ocaml int *)
(*let g2o_int s =*)
    (*match explode s with*)
    (*| '0'::(_::t as o)          -> int_of_string @@ implode ('0'::'o'::o)*)
    (*| '0'::'x'::t | '0'::'X'::t -> int_of_string s*)
    (*| _                         -> int_of_string s*)
