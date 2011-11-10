(* 
   Matthew Hammer <hammer@mpi-sws.org>
   
   LablGtk:
   http://www.math.nagoya-u.ac.jp/~garrigue/soft/olabl/lablgtk.html

   Based on code examples from:   
   http://plus.kaist.ac.kr/~shoh/ocaml/lablgtk2/lablgtk2-tutorial/
*)

open Tv_signal_util
open Tv_mirror

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

module Lablgtk2_util = struct
  
  let text_renderer =
    GTree.cell_renderer_text 
      [`FONT_DESC (Pango.Font.from_string "monospace 8")] 
      
  let make_column 
      (cols : GTree.column_list) 
      (ty : 'a Gobject.data_conv) 
      (title : string) 
      :
      ('a GTree.column * GTree.view_column)
      =
    let c  : 'a GTree.column = cols#add ty in
    let vc : GTree.view_column = 
      GTree.view_column ~title:title ~renderer:(text_renderer, ["text", c]) () 
    in
    (c, vc)

end
open Lablgtk2_util



(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)


type gui_params = {
  fps : float ;
}

module Sm = Map.Make(Mirror.State_id)

type gui_state = {
  mutable frame_timeout_id : GMain.Timeout.id option ;
  mutable mirror_bistream  : Mirror.bistream ;
  mutable channels         : Channel.Kind_set.t ;
  mutable play_state       : [`Playing | `Paused ];

  (* Yuck. *)
  mutable event_list_scrw  : GBin.scrolled_window ;
  mutable event_list_view  : GTree.view ;
  mutable event_list_store : GTree.list_store ;
  mutable event_list_col   : string GTree.column * GTree.view_column ;
  mutable event_list_paths : Gtk.tree_path Sm.t;
}


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

let frame_callback params gui_state () = 
  match gui_state.play_state with
    | `Paused  -> gui_state.frame_timeout_id <- None ; false

    | `Playing -> begin
        
        Printf.eprintf "frame_callback: begin\n%!" ;
        
        gui_state.mirror_bistream <- 
          Mirror.forward 
          gui_state.channels
          gui_state.mirror_bistream 
        ;
        
        Printf.eprintf "frame_callback: end.\n%!" ;
        true
      end


let mirror_callback params gui_state what mirror_bistream =
  Printf.eprintf "mirror_callback: begin\n%!" ;
  let st = Mirror.get_state mirror_bistream in
  
  begin match what with
    | `Get_event ev -> 
        let store = gui_state.event_list_store in
        let iter  = store#append () in
        let path  = store#get_path iter in
        
        ignore ( store#set ~row:iter ~column:(fst gui_state.event_list_col)
                   ( Tv_pretty.string_of_event ev ) ) ;
        
        (* Store the path. *)
        gui_state.event_list_paths <-
          Sm.add (Mirror.get_id (Mirror.get_state gui_state.mirror_bistream)) path
          gui_state.event_list_paths ;

        (* Move the cursor. *)
        gui_state.event_list_view#set_cursor 
          path (snd gui_state.event_list_col) ;
        
        (* Scroll the window. *)
        gui_state.event_list_scrw#vadjustment#set_value 
           gui_state.event_list_scrw#vadjustment#upper ;
        ()
    
    | _ -> ()
  end ;

  Printf.eprintf "mirror_callback: end.\n%!" ;
  mirror_bistream  


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

let ms_of_hz x = (1.0 /. x) *. 1000.0

let create_event_list gui_state =
  (* Create a new scrolled window, with scrollbars only if needed *)
  let scrolled_window = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in  
  let cols  = new GTree.column_list in  
  let col   = make_column cols Gobject.Data.string "CEAL TV Signals" in
  let model = GTree.list_store cols in
  let view  = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in
  view#append_column (snd col) ;
  gui_state.event_list_scrw  <- scrolled_window ;
  gui_state.event_list_view  <- view ;
  gui_state.event_list_store <- model ;
  gui_state.event_list_col   <- col ;
  scrolled_window#coerce

let create_trace_view () =
  (* Create a new scrolled window *)
  let scrolled_window = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in

  (* Create a table of 10 by 10 squares.
   * Set the spacing to 10 on x and 10 on y *)
  let table = GPack.table ~rows:10 ~columns:10 ~row_spacings:0 ~col_spacings:0
    ~packing:scrolled_window#add_with_viewport () in

  for i=0 to 50 do
    for j=0 to 50 do
      let _ = GMisc.label
        ~text:("("^ string_of_int i ^","^ string_of_int j ^")\n")
        ~packing:(table#attach ~left:i ~top:j) ()
      in ()
    done
  done;
  scrolled_window#coerce


let create_vcr_controls params gui_state =
  (* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and finally, we'll also put it into our
   * handlebox *)

  let toolbar1 = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`ICONS
    ~border_width:5 (* ~space_size:5 *)
    ()
  in

  let toolbar2 = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`ICONS
    ~border_width:5
    ()
  in  

  let adjustment = 
    GData.adjustment
      ~value:15.0 ~lower:1.0 ~upper:99.0 ~step_incr:1.0 () in

  let _ = GMisc.label 
    ~text:"FPS " 
    ~packing:toolbar2#add () in

  let spinner = GEdit.spin_button 
    ~adjustment 
    ~packing:toolbar2#add
    ~value:params.fps () in  

  let pause = GButton.radio_tool_button ~label:"pause"
    ~packing:(toolbar1#insert) ~stock:`MEDIA_PAUSE () in
  let _ = pause#connect#clicked 
    (fun _ -> gui_state.play_state <- `Paused) in

  let play  = GButton.radio_tool_button ~label:"play"
    ~packing:(toolbar1#insert) ~stock:`MEDIA_PLAY 
    ~group:pause ()in
  let _ = play#connect#clicked 
    (fun _ -> 
       gui_state.play_state <- `Playing ;
       match gui_state.frame_timeout_id with
         | None ->
             gui_state.frame_timeout_id <- Some (
               GMain.Timeout.add ~ms:(int_of_float (ms_of_hz spinner#value))
                 ~callback:(frame_callback params gui_state)
             )
         | Some _ -> ()
    ) in
  
  let home = GButton.tool_button ~label:"home"
    ~packing:(toolbar1#insert) ~stock:`GOTO_FIRST () in
  let _ = home#connect#clicked (fun _ -> () ) in
  
  let back = GButton.tool_button ~label:"back"
    ~packing:(toolbar1#insert) ~stock:`GO_BACK () in
  let _ = back#connect#clicked (fun _ -> () ) in
  
  let forw = GButton.tool_button ~label:"forw"
    ~packing:(toolbar1#insert) ~stock:`GO_FORWARD () in
  let _ = forw#connect#clicked (fun _ -> () ) in
  
  let endb = GButton.tool_button ~label:"end"
    ~packing:(toolbar1#insert) ~stock:`GOTO_LAST () in
  let _ = endb#connect#clicked (fun _ -> () ) in
  
  ignore ( adjustment#connect#value_changed 
    ~callback:begin fun () -> 
      match gui_state.frame_timeout_id with
        | Some id ->
            GMain.Timeout.remove id ;
            gui_state.frame_timeout_id <- Some (
              GMain.Timeout.add ~ms:(int_of_float (ms_of_hz spinner#value))
                ~callback:(frame_callback params gui_state)
            )
        | None -> ()
    end ) ;
  
  
  let hbox = GPack.hbox () in
  hbox#add toolbar1#coerce ;
  hbox#add toolbar2#coerce ;

  ( hbox#coerce )


let dummy_event_list _ =
  let cols  = new GTree.column_list in
  let col   = make_column cols Gobject.Data.string "" in
  let store = GTree.list_store cols in
  let view  = GTree.view ~model:store () in
  (store, cols, col, view)

let main params = (
  
  let gui_state =     
    (* These are temporary things. *)
    let dum_scrw = GBin.scrolled_window () in
    let dum_store, dum_cols, dum_col, dum_view = dummy_event_list () in
    { 
      frame_timeout_id = None ;
      play_state       = `Paused ;
      mirror_bistream  = Mirror.create {
        Mirror.get_event = (
          fun () ->
            try
              let event  = Tv_signal.event_of_string (read_line ()) in
              Some ( event )
            with
              | End_of_file -> None
        ) ;
        Mirror.callback = None
      } ;
      channels         = Channel.universal_kind_set ;

      event_list_paths = Sm.empty ;
      event_list_scrw  = dum_scrw ;
      event_list_col   = dum_col ;
      event_list_store = dum_store ;
      event_list_view  = dum_view ;
    }
  in
  
  (* Tie the recursive knot. *)
  gui_state.mirror_bistream <- 
    Mirror.set_callback (Some (mirror_callback params gui_state))
    gui_state.mirror_bistream ;

  (* Create a new window; set title and border width *)
  let window = GWindow.window ~title:"CEAL Trace Visualizer" ~border_width:0
    ~width:400 ~height:600 () in

  (* Set a handler for destroy event that immediately exits GTK. *)
  ignore (window#connect#destroy ~callback:GMain.Main.quit);

  let vbox = GPack.vbox ~packing:window#add () in
  
  let vcr_controls = create_vcr_controls params gui_state in
  vbox#add vcr_controls;
  vbox#set_child_packing ~expand:false vcr_controls;

  let vpaned = GPack.paned `VERTICAL ~packing:vbox#add () in

  let event_list = create_event_list gui_state in
  vpaned#add2 event_list;
  vpaned#pack2 ~resize:false event_list;

  let trace_view = create_trace_view () in
  vpaned#add1 trace_view;
  vpaned#pack1 ~resize:true trace_view;
  
  window#show ();

  GMain.Main.main ()
)

let _ = Printexc.print main { fps = 5.0 }
