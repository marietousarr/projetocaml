open Graph

(* Create a new graph with the same nodes but without edges *)
val clone_nodes: 'a graph -> 'b graph

(* Map on an a' graph *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Add an edge in the graph *)
val add_arc: int graph -> id -> id -> int -> int graph
