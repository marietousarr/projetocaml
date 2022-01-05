open Gfile
open Tools
open Fulkerson

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let gr = from_file infile in

  let gr = (gmap gr (int_of_string)) in

  (*let ec = create_ecart gr in
    let path1= (find_path ec 0 3 [0]) in

    let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x)::rest -> Printf.printf ("%d ,") x; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");
    in loop path1;

    let flot = find_min ec path1 max_int in 
    (match flot with 
    |(a) ->  Printf.printf " flot min du graphe d'ecart %d\n " a);
    let ec2 = saturer ec flot path1 in 

    let path1= (find_path ec2 0 3 [0]) in

    let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x)::rest -> Printf.printf ("%d ,") x; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");
    in loop path1;

    let flot = find_min ec2 path1 max_int in 
    (match flot with 
    |(a) ->  Printf.printf " flot min du graphe d'ecart %d\n " a);
    let ec2 = saturer ec2 flot path1 in 

    let path1= (find_path ec2 0 3 [0]) in
    let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x)::rest -> Printf.printf ("%d ,") x; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");
    in loop path1;

    let flot = find_min ec2 path1 max_int in 
    (match flot with 
    |(a) ->  Printf.printf " flot min du graphe d'ecart %d\n " a);
    let ec2 = saturer ec2 flot path1 in 


    let path1= (find_path ec2 0 3 [0]) in
    let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x)::rest -> Printf.printf ("%d ,") x; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");
    in loop path1;

    let flot = find_min ec2 path1 max_int in 
    (match flot with 
    |(a) ->  Printf.printf " flot min du graphe d'ecart %d\n " a);
    let ec2 = saturer ec2 flot path1 in 
  *)

  let (ec2, flot) = ford_fulkerson gr _source _sink
  in 

  (* Rewrite the graph that has been read. *)
  let gr = gmap ec2 (flot_to_string) in
  let () = write_file outfile gr; in
  export gr "./graphs/res.gv";
  Printf.printf "Le flot max est %d" flot;
  ()
