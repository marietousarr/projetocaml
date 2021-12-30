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

  let path1= (find_path gr 0 5)in

  let rec loop p = (match p with 
      |Some a -> (match a with 
          |(x,b,c)::rest -> Printf.printf ("%d %d ,") x b; loop (Some rest)
          |[] -> Printf.printf " fin\n"
        )
      |None -> Printf.printf "rien \n");

  in loop path1;

  (match (find_min gr path1 (max_int,0)) with 
   |(a,b) ->  Printf.printf " flot min %d,%d\n " a b);




    (*
      let graph = (gmap gr (int_of_string)) 
      in
      let (gr1, a) = ford_fulkerson graph _source _sink
      in *)
  let gr2 = (gmap gr (string_of_int)) 

  (* Rewrite the graph that has been read. *)
  in
  let () = write_file outfile gr2 in
  export gr2 "./graphs/format.gv";
  ()
