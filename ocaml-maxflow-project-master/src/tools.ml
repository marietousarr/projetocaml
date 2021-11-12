(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = 
  let rec loop graphacc mygr = match mygr with 
    |[]-> graphacc
    |(id, _):: rest -> loop (new_node mygraph id) rest
  in loop (empty_graph) gr

(*
let gmap gr f = 
  let mygraph = empty_graph
  in 
    let rec loop mygr = match mygr with 
      |[]-> mygraph
      |(id1, out_arcs):: rest -> match out_arcs with
                        |[] -> loop rest
                        |(id2, lbl)::rest2 -> (new_arc mygraph id1 id2 (f lbl)); 

    in loop gr

    *)