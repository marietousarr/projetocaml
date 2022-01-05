open Graph
open Gfile

type graph_path = (id list) option

type flot_label= {flot:int; capacite:int} 

val create_ecart : int graph -> int graph

val find_path: int graph -> id -> id -> int list -> graph_path

val process_outarcs : int graph -> id -> int out_arcs -> id -> int list -> graph_path

val find_min: int graph-> graph_path -> int-> int

val saturer : int graph -> int -> graph_path -> int graph

val ecart_to_flot: int graph -> int graph -> flot_label graph

val flot_to_string: flot_label->string

val flot_max: flot_label graph->id->int

val ford_fulkerson: int graph -> id -> id -> (flot_label graph) * int