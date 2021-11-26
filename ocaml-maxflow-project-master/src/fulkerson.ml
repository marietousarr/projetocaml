open Graph
open Gfile
open Tools
open Printf

let create_ecart gr=
  let gr2=clone_nodes gr 
  in
  e_fold gr (fun gr id1 id2 lbl-> new_arc (new_arc gr id1 id2 lbl) id2 id1 0) gr2
