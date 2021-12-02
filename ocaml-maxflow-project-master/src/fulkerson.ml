open Graph
open Gfile
open Tools
open Printf


type 'a graph_path = ((id * id * 'a) list) option

let create_ecart gr=
  let gr2=clone_nodes gr 
  in
  e_fold gr (fun gr id1 id2 lbl-> new_arc (new_arc gr id1 id2 lbl) id2 id1 0) gr2


let rec process_outarcs gr id lo dest = match lo with
  |(idx, lbl)::rest -> (let res = find_path gr idx dest 
                        in 
                        if (res) == None then (process_outarcs gr id rest dest) 
                        else match res with 
                          |Some a -> Some ((id,idx,lbl)::a)
                          |None -> Some [(id,idx,lbl)]
                       )
  |[]-> None 

and find_path gr id1 id2 = 
  if id1 == id2 
  then Some []
  else
    process_outarcs gr id1 (out_arcs gr id1) id2

(*let ford_fulkerson gr s p = 
  let gec = create_ecart gr in let loop *)