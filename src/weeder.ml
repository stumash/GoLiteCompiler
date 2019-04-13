open Tree
open Helpers

let break_continue_msg = "break and continue statements cannot be used outside for-loops"

let rec wd_prog prog =
    match prog with    
    | Program (p, dl) -> List.iter wd_decl dl
    | EmptyProgram    -> ()

and wd_decl d = 
    match d with 
    | FunctionDeclaration (i, para, tso, ss, pos) -> 
        let f s =
            let s_type = wd_stmt s in 
            (match s_type with 
            | Continue pos | Break pos ->
                raise (BreakContinueError (break_continue_msg, pos))
            | _ -> ()) in
        List.iter f ss
    | _ -> ()

and wd_stmt stmt =
    match stmt with 
    | Break pos -> Break pos
    | Continue pos -> Continue pos
    | BlockStatements (ss, pos) ->
        let f s =
            let s_type = wd_stmt s in
            (match s_type with 
            | Continue pos | Break pos ->
                raise (BreakContinueError (break_continue_msg, pos))
            | _ -> ()) in
        List.iter f ss;
        EmptyStatement
    | ForStatement (s1, eo, s2, ss, pos, _) -> List.iter (fun s -> wd_stmt s; ()) ss; EmptyStatement
    | SwitchStatement (s, eo, scl, pos) -> List.iter (fun sw -> wd_sc sw; ()) scl; EmptyStatement
    | IfStatement ifs ->
        (match ifs with 
        | If (s, e, ss, eso, pos) as ifstmt -> wd_ifst ifstmt 
        | _ -> ());
        EmptyStatement     
    | _ -> EmptyStatement

and wd_sc sw = 
    match sw with 
    | Default (ss, pos) ->
        let f s = 
            let s_type = wd_stmt s in
            (match s_type with 
            | Continue pos ->
                raise (BreakContinueError (break_continue_msg, pos))
            | _ -> ()) in
        List.iter f ss
    | Case (el, ss, pos) ->
        let f s =
            let s_type = wd_stmt s in
            (match s_type with 
            | Continue pos ->
                raise (BreakContinueError (break_continue_msg, pos))
            | _ -> ()) in
        List.iter f ss    

and wd_ifst (If (s, e, ss, eso, pos)) = 
    let f s =
        let s_type = wd_stmt s in 
        (match s_type with 
        | Break pos | Continue pos ->
            raise (BreakContinueError (break_continue_msg, pos))
        | _ -> ()) in
    List.iter f ss;

    (match eso with 
    | Some els -> 
        (match els with 
        | Elseif (ifs, pos) -> wd_ifst ifs
        | Else (ss, pos) ->
            let f s =
                let s_type = wd_stmt s in
                (match s_type with 
                | Break pos | Continue pos ->
                    raise (BreakContinueError (break_continue_msg, pos))
                | _ -> ()) in
            List.iter f ss)
    | None -> ());
    ()
