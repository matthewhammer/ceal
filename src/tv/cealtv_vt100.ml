open Tv_signal_util
open Tv_mirror
open Tv_surface
open Tv_trace_surface

module Flags = struct
  let term_size     = ref (65,120)
  let event_len     = ref 10
end

type play_state = Playing | Paused

type st = {
  mirror     : Mirror.stream ;
  channels   : Channel.Kind_set.t ;
  play_state : play_state ;
  play_fps   : float ;
  print_st   : Tv_pretty.Print.t ;
}

let channels = 
  List.fold_right Channel.Kind_set.add
    [
      Channel.Sasm_eval_step ;
      Channel.Sasm_undo_step ;
      Channel.Meta_step ;
    ]
    Channel.Kind_set.empty

let initial_state get_ev = {
  mirror     = Mirror.create get_ev ;
  channels   = Channel.universal_kind_set ;
  play_state = Playing ;
  play_fps   = 35.0 ;
  print_st   = Tv_pretty.Print.defaults ;
}

(* Pause until its time for the next frame. *)
let pause st = 
  let time = Unix.gettimeofday () in
  let delay = 1.0 /. st.play_fps in
  while Unix.gettimeofday () -. time < delay do
    ignore (Unix.select [] [] [] 0.005)
  done
      
(* Print the Trace Zipper. *)      
let show_trace_zipper st =
  let mirror_st     = Mirror.get_state st.mirror in
  let tz            = Mirror.get_tz mirror_st in
  let trace_surf    = ( Trace_surface.zipper 
                          ( Mirror.get_last_mode mirror_st )
                          ( Mirror.get_action_props mirror_st ) tz Surface.empty ) in
  let line_end      = Tv_pretty.erase_west ^ "\n" in
  let vt100         = Trace_surface.Pretty.vt100 trace_surf (!Flags.term_size) " " line_end in
  let _             = print_string Tv_pretty.home in  
  let _             = print_string (Buffer.contents vt100) in
  ( st )

let show_event_list st =
  let st = (* TEMP *) {st with print_st = Tv_pretty.Print.defaults } in 

  let past, future = Mirror.get_events (!Flags.event_len) st.mirror in
  let print = List.fold_left (
    fun st ev -> 
      let print_st, str = Tv_pretty.Print.string_of_event st.print_st ev in
      let _ = Printf.printf "%s%s\n" str Tv_pretty.erase_west in
      { st with print_st = print_st }
  ) 
  in
  let st = print st past in
  let st = print st future in
  ( st )

(* Get the next event from stdin *)
let get_next_event = 
  (*let dump = ref Tv_pretty.Print.defaults in*)
  fun () ->
    try
      let event         = Tv_signal.event_of_string (read_line ()) in
      (*let dump', ev_str = Tv_pretty.Print.string_of_event (!dump) event in
      let _             = dump := dump' in
      let _             = print_string (ev_str^"\n") in
      let _             = flush stdout in *)
      Some ( event )
    with
      | End_of_file -> None      

let rec loop st =

  (* Change state according to the key press. *)
  (*
  let st = 
    if Graphics.key_pressed () then (
      match Graphics.read_key () with
        | char -> 
            print_string ("key: `" ^ (string_of_int (int_of_char char)) ^ "'\n") ; 
            st
    )
    else
      ( st )
  in
  *)

  (* Change state according to current state. *)
  let st =     
    match st.play_state with
      | Playing -> 
          { st with mirror = Mirror.forward st.channels st.mirror }

      | Paused  -> st
  in
  
  let st = show_trace_zipper st in

  let st = show_event_list st in

  print_string Tv_pretty.erase_down ;
  flush stdout ;
  pause st ;
  loop st

let _ = 
  (* let _ = Graphics.open_graph ":0" in *)
  loop (initial_state get_next_event)
