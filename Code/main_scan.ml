open Printf
open Salex

let main () =
  let lexbuf = Lexing.from_channel stdin in
  count lexbuf;
  Printf.printf "# of lines = %d, # of chars = %d\n" !num_lines !num_chars

let _ = Printexc.print main ()
  