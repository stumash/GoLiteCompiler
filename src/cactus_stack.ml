module T = Golitetypes

(* Cactus-Stack Node*)
type cs_node =
    | Empty
    | CtNode of
        cs_node *                              (* parent *)
        cs_node list *                         (* children *)
        (string, (T.vcat, T.gltype)) Hashtbl.t (* data *)

