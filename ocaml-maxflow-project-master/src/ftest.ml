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

  (* Convert the string graph into an int graph *)
  let gr = (gmap gr (int_of_string)) in

  (* Apply the fold fulkerson algorithm *)
  let (gr_flot, flot) = ford_fulkerson gr _source _sink in 

  (* Write the final flow graph *)
  let gr = gmap gr_flot (flot_to_string) in
  let () = write_file outfile gr; in
  (* Export the final flow graph into dot format*)
  export_ff gr _source _sink "./graphs/result.gv";
  (* Print the flow max*)
  Printf.printf "\nThe maximal flow of this graph from Node %d to Node %d is %d !\n" _source _sink flot;
  ()
