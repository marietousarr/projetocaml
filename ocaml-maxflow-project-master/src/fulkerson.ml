open Graph
open Gfile
open Tools
open Printf


type 'a graph_path = ((id * id * 'a) list) option

let create_ecart gr=
  let gr2=clone_nodes gr 
  in
  e_fold gr (fun gr id1 id2 lbl-> new_arc (new_arc gr id2 id1 lbl) id1 id2 0) gr2


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

let rec find_min gr pat acu= match pat with
  |Some ((id1,id2,a)::[]) -> (match (find_arc gr id1 id2) with
      |Some lbl -> (match acu with 
          |(x,_) -> if (lbl<x) then (lbl, (lbl-a)) else acu)(* ( capacité min ,valeur a ajouter) *)
      |_->acu (* on y arrive jamais *))
  |Some ((id1,id2,a)::q)-> (match (find_arc gr id1 id2) with
      |Some lbl->(match acu with 
          |(x,_) -> if lbl<x then find_min gr (Some q) (lbl, (lbl-a)) else find_min gr (Some q) acu)
      |_->acu (* on y arrive jamais *))
  |Some [] -> (0,-1)
  |None-> (-1, -1)


let rec saturer ecart flot_min path = match flot_min with |(_,y) ->
match path with 
(* |Some ((id1,id2,a)::[]) -> new_arc ecart id1 id2 (a+y) *)
|Some ((id1,id2,a)::q)-> saturer (new_arc ecart id1 id2 (a+y)) flot_min (Some q)
|Some [] -> ecart
|None-> ecart 


(* enlever le Some du path avant de faire find_min  *)
let ford_fulkerson gr s p = 
  let ecart = create_ecart gr in 
  let rec loop ecart= 
    let pat = (find_path ecart s p) in
    let augment = find_min gr pat (max_int, 0) in 
    match augment with
    |(x,y) ->  if (y == -1) 
      then (ecart, 100)
      else loop (saturer ecart augment pat)
  in loop ecart