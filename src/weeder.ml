open Tree
open Helpers

let rec wd_prog prog =
  match prog with  
  | Program (p, dl) -> List.iter wd_decl dl 
  | EmptyProgram -> () 

and wd_decl d = 
  match d with 
  | FunctionDeclaration (i, para, tso, ss, pos) -> 
    List.iter (fun s ->  let s_type = wd_stmt s in 
                  (match s_type with 
                  | Continue pos | Break pos -> raise (BreakContinueError ("Break or Continue cannot be for a function",pos))
                  | _ -> ())) ss
  | _ -> ()

and wd_stmt stmt =
  match stmt with 
  | Break pos -> Break pos
  | Continue pos -> Continue pos
  | BlockStatements (ss, pos) -> List.iter (fun s -> let s_type = wd_stmt s in
                            (match s_type with 
                            | Continue pos | Break pos -> raise (BreakContinueError ("Break or Continue cannot be for a function",pos))
                            | _ -> ())) ss; EmptyStatement
  | ForStatement (s1, eo, s2, ss, pos) -> List.iter (fun s -> wd_stmt s; ()) ss; EmptyStatement
  | SwitchStatement (s, eo, scl, pos) -> List.iter (fun sw -> wd_sc sw; ()) scl; EmptyStatement
  | IfStatement ifs -> (match ifs with 
                          | If (s, e, ss, eso, pos) as ifstmt -> wd_ifst ifstmt 
                          | _ -> ()); EmptyStatement   
  | _ -> EmptyStatement

and wd_sc sw = 
  match sw with 
  | Default (ss, pos) -> List.iter (fun s -> let s_type = wd_stmt s in
                    (match s_type with 
                    | Continue pos -> raise (BreakContinueError ("Continue cannot be for a switch clause",pos))
                    | _ -> ())) ss
  | Case (el, ss, pos) ->  List.iter (fun s -> let s_type = wd_stmt s in
                        (match s_type with 
                        | Continue pos-> raise (BreakContinueError ("Continue cannot be for a switch clause",pos))
                        | _ -> ())) ss  

and wd_ifst (If (s, e, ss, eso, pos)) = 
  List.iter (fun s -> let s_type = wd_stmt s in 
              (match s_type with 
              | Break pos| Continue pos -> raise (BreakContinueError ("Break or Continue cannot be for an if clause",pos))
              | _ -> () )) ss;
  (match eso with 
  | Some els -> 
                  (match els with 
                  | Elseif (ifs, pos) -> wd_ifst ifs
                  | Else (ss, pos) -> List.iter (fun s -> let s_type = wd_stmt s in
                                (match s_type with 
                                | Break pos | Continue pos-> raise (BreakContinueError ("Break or Continue cannot be for an else clause",pos))
                                | _ -> () )) ss)
  | None -> ());
  ()

