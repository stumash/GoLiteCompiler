(* String of Go Int To Int *)
(* sgi - String repr. of Go Int *)
(* soi - String repr. of OCaml Int *)
let sgi2i sgi =
    let soi =
        let len = String.length sgi in
        if len >= 2 && sgi.[0] == '0' && sgi.[1] != 'X' && sgi.[1] != 'x' then
            "0o" ^ (String.sub sgi 1 (len-1))
        else sgi in
    print_endline soi; int_of_string soi

(* String of Go Float to Float *)
(* Go float syntax is compatible with OCaml float syntax *)
let sgf2f = float_of_string

(* String of Go Boolean to Boolean *)
(* Go boolean syntax is compatible with OCaml boolean syntax *)
let sgb2b = bool_of_string