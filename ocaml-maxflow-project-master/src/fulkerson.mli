open Graph
open Gfile

type 'a graph_path = ((id * id * 'a) list) option

val create_ecart : int graph -> int graph

val find_path: int graph -> id -> id -> int graph_path

val process_outarcs : int graph -> id -> int out_arcs -> id -> int graph_path

val find_min: int graph-> int graph_path -> (int * int)-> (int * int)

val saturer : int graph -> int * int -> int graph_path -> int graph

val ford_fulkerson: int graph -> id -> id -> int graph*int