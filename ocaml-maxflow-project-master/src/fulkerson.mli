open Graph
open Gfile

type graph_path = (id list) option

type flot_label= {flot:int; capacite:int} 

(* Crée le graphe d'écart *)
val create_ecart : int graph -> int graph

(* Find a path between source and destination*)
val find_path: int graph -> id -> id -> int list -> graph_path

(* try to find a path from src to dest starting with an arc of the 'partial' list of out_arcs of src*)
val process_outarcs : int graph -> id -> int out_arcs -> id -> int list -> graph_path

(* Find the minimal flow we can add to the arcs of the path*)
val find_min: int graph-> graph_path -> int-> int

(* Add a value (flot_min) to each edge of the path path*)
val saturer : int graph -> int -> graph_path -> int graph

(* Convert an int graph) into a flot_label graph given his graphe d'écart*)
val ecart_to_flot: int graph -> int graph -> flot_label graph

(* Convert a flot_label into type string*)
val flot_to_string: flot_label->string

(* Return the flow arriving at the sink p of a flot_label graph*)
val flot_max: flot_label graph-> id ->int

(* the actual algorithm *)
val ford_fulkerson: int graph -> id -> id -> (flot_label graph) * int