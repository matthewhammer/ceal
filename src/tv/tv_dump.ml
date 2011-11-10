open Printf
module Tvs = Tv_signal

let _ =  
  let rec loop params = 
    let params = try
      let event  = Tv_signal.event_of_string (read_line ()) in
      let params = Tv_pretty.Print.print_event params event in
      print_string "\n" ;
      flush stdout ;
      params
  with 
      End_of_file -> exit 0
    in
    loop params
  in
  loop Tv_pretty.Print.defaults
