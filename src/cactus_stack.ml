module T = Golitetypes

(* Cactus-Stack Node*)
type cs_node =
    | CsRoot
    | CsNode of {
        parent   : cs_node;
        children : cs_node list ref;
        context  : (string, (T.vcat * T.gltype)) Hashtbl.t;
      }

exception CsRootHasNoContents

let parent csn =
    match csn with
    | CsRoot -> raise CsRootHasNoContents
    | CsNode {parent; children; context} -> parent

let children csn =
    match csn with
    | CsRoot -> raise CsRootHasNoContents
    | CsNode {parent; children; context} -> children

let context csn =
    match csn with
    | CsRoot -> raise CsRootHasNoContents
    | CsNode {parent; children; context} -> context

let add_child_scope csn_parent csn_child =
    match csn_parent, csn_child with
    | CsNode s_p, CsNode _ -> s_p.children :=  csn_child::!(s_p.children)
    | _,_ -> raise CsRootHasNoContents

let print_cs_node csn =
    let spaces n = String.make (n*4) ' ' in
    let p_tbl tbl indent =
        let p_kv str (cat,glt) =
            print_endline ((spaces indent)^str^": "^(T.string_of_vcat cat)^","^(T.string_of_glt glt)) in
        Hashtbl.iter p_kv tbl
    in
    let rec print_cs_node' indent csn =
        let cxt = context csn in
        let kids = !(children csn) in
        p_tbl cxt indent;
        print_endline ((spaces indent)^"--->");
        List.iter (print_cs_node' (indent+1)) (List.rev kids);
        print_endline ((spaces indent)^"<---");
    in
    print_cs_node' 0 csn 
