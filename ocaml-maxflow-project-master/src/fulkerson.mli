open Graph
open Gfile

type 'a graph_path = ((id * id * 'a) list) option

val create_ecart : int graph -> int graph

val find_path: 'a graph -> id -> id -> 'a graph_path

val process_outarcs : 'a graph -> id -> 'a out_arcs -> id -> 'a graph_path

val find_min: int graph-> int graph_path -> int-> int

val ford_fulkerson: int graph -> id -> id -> int graph*int