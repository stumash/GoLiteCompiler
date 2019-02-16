open Printf
open Salex

let main () =
  let lexbuf = Lexing.from_channel stdin in
  while true do 
    scanner lexbuf;
  done
  

let _ = Printexc.print main ()
  