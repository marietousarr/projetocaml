open Graph
open Gfile
open Tools
open Printf
type graph_path = (id list) option

let create_ecart gr=
  let gr2=clone_nodes gr 
  in
  e_fold gr (fun gr id1 id2 lbl-> new_arc (new_arc gr id2 id1 0) id1 id2 lbl) gr2


let rec find_path gr src dest acu = 
  if (src == dest) 
  then Some (List.rev acu)
  else
    process_outarcs gr src (out_arcs gr src) dest acu

and process_outarcs gr src lo dest acu = match lo with
  |[] -> None
  |(id, lbl)::tail -> if (lbl == 0)
    then process_outarcs gr src tail dest acu
    else if (List.mem id acu) 
    then process_outarcs gr src tail dest acu
    else find_path gr id dest (id::acu)



let rec find_min gr path acu = match path with
  |Some (id1::(id2::t)) -> (let lab = find_arc gr id1 id2 in 
                            match lab with 
                            |None -> failwith "Le chemin est incorrect"
                            |Some v -> if (v < acu)
                              then find_min gr (Some (id2::t)) v
                              else find_min gr (Some (id2::t)) acu
                           )
  |Some [id] -> acu
  |None -> -1 (* pas de chemin *)
  |Some [] -> 0 (* src = dest *)


let rec saturer ecart flot_min path = match path with 
  |None -> failwith "pas de chemin"
  |Some x -> (match x with 
      |id1::(id2::t) -> let ajout = find_arc ecart id1 id2 and retrait = find_arc ecart id2 id1 in
        (match (ajout, retrait) with 
         |(None, _) -> failwith "chemin incorrect"
         |(_,None) ->  failwith "chemin incorrect"
         | (Some a, Some r) -> saturer (new_arc (new_arc ecart id1 id2 (a-flot_min)) id2 id1 (r+flot_min)) flot_min (Some (id2::t))
        )
      |[id] -> ecart
      |[] -> ecart  
    )



let ford_fulkerson gr s p= 
  let ec = create_ecart gr in 
  let rec loop ecart= 
    let pat = (find_path ecart s p [s]) in
    let augment = find_min ecart pat max_int in 
    if (augment == 0) 
    then (ecart, 0)
    else if (augment == -1)
    then (ecart, 100)
    else loop (saturer ecart augment pat)
  in loop ec