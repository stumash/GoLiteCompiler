(* main *)
let () =
  let lexbuf = Lexing.from_channel stdin in
  while true do 
    Salex.scanner lexbuf;
  done
