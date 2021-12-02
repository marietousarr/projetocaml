open Graph
open Gfile

type 'a graph_path = ((id * id * 'a) list) option

val create_ecart : int graph -> int graph

val find_path: 'a graph -> id -> id -> (id * id * 'a) list option

val process_outarcs : 'a graph -> id -> 'a out_arcs -> id -> (id * id * 'a) list option

(*val ford_fulkerson: int graph -> id -> id -> int graph*int*)