(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun gr id1 id2 lbl ->  new_arc gr id1 id2 (f lbl)) (clone_nodes gr)

let add_arc g id1 id2 n = 
  let a = find_arc g id1 id2 
  in match a with 
  |None -> new_arc g id1 id2 n
  |Some x-> new_arc g id1 id2 (n+x)