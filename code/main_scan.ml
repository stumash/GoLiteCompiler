(* main *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do 
    Lexer.scanner lexbuf;
  done
