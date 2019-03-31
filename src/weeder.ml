open Tree
open Helpers

let rec wd_prog prog =
  match prog with  
  | Program (p, dl) -> List.iter wd_decl dl 
  | EmptyProgram -> () 

and wd_decl d = 
  match d with 
  | FunctionDeclaration (i, para, tso, ss) -> 
    List.iter (fun s ->  let s_type = wd_stmt s in 
                  (match s_type with 
                  | Continue | Break -> raise (BreakContinueParserError "Break or Continue cannot be for a function")
                  | _ -> ())) ss
  | _ -> ()

and wd_stmt stmt =
  match stmt with 
  | Break -> Break
  | Continue -> Continue
  | BlockStatements ss -> List.iter (fun s -> let s_type = wd_stmt s in
                            (match s_type with 
                            | Continue | Break -> raise (BreakContinueParserError "Break or Continue cannot be for a function")
                            | _ -> ())) ss; EmptyStatement
  | ForStatement (s1, eo, s2, ss) -> List.iter (fun s -> wd_stmt s; ()) ss; EmptyStatement
  | SwitchStatement (s, eo, scl) -> List.iter (fun sw -> wd_sc sw; ()) scl; EmptyStatement
  | IfStatement ifs -> (match ifs with 
                          | If (s, e, ss, eso) as ifstmt -> wd_ifst ifstmt 
                          | _ -> ()); EmptyStatement   
  | _ -> EmptyStatement

and wd_sc sw = 
  match sw with 
  | Default ss -> List.iter (fun s -> let s_type = wd_stmt s in
                    (match s_type with 
                    | Continue -> raise (BreakContinueParserError "Continue cannot be for a switch clause")
                    | _ -> ())) ss
  | Case (el, ss) ->  List.iter (fun s -> let s_type = wd_stmt s in
                        (match s_type with 
                        | Continue -> raise (BreakContinueParserError "Continue cannot be for a switch clause")
                        | _ -> ())) ss  

and wd_ifst (If (s, e, ss, eso)) = 
  List.iter (fun s -> let s_type = wd_stmt s in 
              (match s_type with 
              | Break | Continue -> raise (BreakContinueParserError "Break or Continue cannot be for an if clause")
              | _ -> () )) ss;
  (match eso with 
  | Some els -> 
                  (match els with 
                  | Elseif ifs -> wd_ifst ifs
                  | Else ss -> List.iter (fun s -> let s_type = wd_stmt s in
                                (match s_type with 
                                | Break | Continue -> raise (BreakContinueParserError "Break or Continue cannot be for an else clause")
                                | _ -> () )) ss)
  | None -> ());
  ()

