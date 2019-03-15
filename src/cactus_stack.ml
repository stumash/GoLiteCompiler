module T = Golitetypes

(* Cactus-Stack Node*)
type cs_node =
    | CsRoot
    | CsNode of {
        parent   : cs_node;
        children : cs_node list ref;
        context  : (string, (T.vcat * T.gltype)) Hashtbl.t
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

let print_cs_node csn =
    () (* TODO *)
