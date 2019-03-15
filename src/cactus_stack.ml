module T = Golitetypes

(* Cactus-Stack Node*)
type cs_node =
    | CsRoot
    | CsNode of {
        parent   : cs_node;
        children : ref cs_node list;
        context  : (string, (T.vcat, T.gltype)) Hashtbl.t
      }

let print_cs_node csn =
    () (* TODO *)
