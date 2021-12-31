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

  (*let (res, flot) = ford_fulkerson gr _source _sink*)

  let path1= (find_path gr 0 3) in
  let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x,b,c)::rest -> Printf.printf ("%d %d flot %d,") x b c; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");

  in loop path1;
  let ec = create_ecart gr in let flot = find_min ec path1 (max_int,0) in 
  (match flot with 
   |(a,b) ->  Printf.printf " flot min du graphe d'ecart %d,%d\n " a b);
  let ec2 = saturer ec flot path1 in 




    (*
      let graph = (gmap gr (int_of_string)) 
      in
      let (gr1, a) = ford_fulkerson graph _source _sink
      in *)
  (*let gr2 = (gmap gr (string_of_int)) in *)

  (* Rewrite the graph that has been read. *)
  let gr = gmap ec2 (string_of_int) in
  let () = write_file outfile gr; in
  export gr "./graphs/res.gv";
  (*Printf.printf "Le flot max est %d" flot;*)
  ()
