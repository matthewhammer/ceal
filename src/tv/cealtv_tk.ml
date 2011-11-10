(* 
   Matthew Hammer <hammer@mpi-sws.org>

   Based on code examples from François Thomasset :
   -- http://www-rocq.inria.fr/~thomasse/Labltk/Tutoriel_FT/
   -- http://www-rocq.inria.fr/~thomasse/Labltk/Tutoriel_FT/basics.html
*)

open Tv_signal_util
open Tk ;;

let top = openTk () ;;
Wm.title_set top "CEAL Trace Visualizer" ;;

Wm.geometry_set top "850x700"


module Channel_gui = struct
  include Channel
    
  type t = {
    frame  : Widget.frame Widget.widget ;
    chans  : chan list 
  }
      
  and chan = {
    chan_kind  : kind ;
    chan_var   : Textvariable.textVariable ;
    chan_frame : Widget.frame Widget.widget ;
  }

  let create top : t =
    let frame = Frame.create ~background:`Red top in

    let create_channel index chan_kind =
      let var = Textvariable.create () in
      let _   = Textvariable.set var "full" in
      let tog,_ = Optionmenu.create frame var ["full"; "edges"; "mute"] in
      let lab = Label.create ~text:(string_of_kind chan_kind) frame in
      let frm = Frame.create frame ~background:`Black in
      ( 
        grid ~column:0 ~row:index ~sticky:"w" [lab] ;
        grid ~column:1 ~row:index [tog] ;
        grid ~column:2 ~row:index ~sticky:"e" [frm] 
      ) 
      ;
      { chan_kind  = chan_kind ;
        chan_frame = frm ;
        chan_var   = var } 
    in

    let rec loop i = function
      | chan_kind :: more ->
          create_channel i chan_kind ::
            ( loop (i + 1) more )
      
      | [] -> []
    in
    { frame = frame ;
      chans = loop 0 Channel.all_kinds }

end


module Event_view = struct

  module Score = struct
    
    type view = {
      name : string ;
    }
  end

  module List = struct
  
    type t = { frame : Widget.frame Widget.widget ;
               listbox : Widget.listbox Widget.widget ; }
        
    let create top : t = 
      let frame1 = Frame.create top in
      let scr_y = Scrollbar.create frame1 in    
      let frame2 = Frame.create frame1 in
      let scr_x = Scrollbar.create ~orient:`Horizontal frame2 in 
      let listbox = Listbox.create ~selectmode:`Extended 
        ~xscrollcommand:(Scrollbar.set scr_x)
        ~yscrollcommand:(Scrollbar.set scr_y) 
        ~foreground:`White
        ~background:`Black
        frame2
      in
      begin
        Scrollbar.configure ~command:(Listbox.yview listbox) scr_y ;
        Scrollbar.configure ~command:(Listbox.xview listbox) scr_x ;
        pack [scr_y]   ~side:`Left    ~fill:`Y;
        pack [frame2]  ~side:`Right   ~fill:`Both ~expand:true;
        pack [scr_x]   ~side:`Bottom  ~fill:`X;
        pack [listbox] ~side:`Top     ~fill:`Both ~expand:true;
        { 
          frame = frame1 ;
          listbox = listbox ;
        }
      end
  end
end


let canvas = Canvas.create
  ~relief:`Raised
  ~background:`Black 
  top ;;  

let start_draw_cb_1 ms =
  
  let delay_state = ref ms
  and timer_state = ref (Timer.add ~ms:0 ~callback:(fun _ -> ()))
  and color_state = ref `Black
  and tagid_state = ref ([] : Tk.tagOrId list)
  in

  let rec draw_cb = 

    fun () ->
      let color_next = function
        | (`Color _ ) -> `Black
        | `Black   -> `Green
        | `Green   -> `Red
        | `Red     -> `Yellow
        | `Yellow  -> `Blue
        | `Blue    -> `White
        | `White   -> `Black
      in
      let color = !color_state in
      let _ = color_state := color_next color in      
      let skip = 16 in      
      let old_tagid_state = ! tagid_state in 
      
      tagid_state := [] ;
      for i = 0 to 10 do
        for j = 0 to 10 do
          tagid_state := 
            ( Canvas.create_text
                ~x:(i*skip) ~y:(j*skip)
                ~fill:color
                ~anchor:`Nw
                ~text:"R" ~tags:["0x00001"]
                canvas ) :: ! tagid_state 
        done
      done ;

      Canvas.delete canvas old_tagid_state ;
      Timer.remove !timer_state ;
      timer_state := Timer.add ~ms:(!delay_state) ~callback:draw_cb ;
  in
  timer_state := Timer.add ~ms:ms ~callback:draw_cb ;
  ( delay_state, timer_state ) ;;



let button_grid = Frame.create
  ~background:`Green
  top ;;

let start_draw_cb_2 ms =
  
  let delay_state = ref ms
  and timer_state = ref (Timer.add ~ms:0 ~callback:(fun _ -> ()))
  and color_state = ref `Black
  and tagid_state = ref ([] : Widget.button Widget.widget list)
  in

  let rec draw_cb = 

    fun () ->
      let color_next = function
        | (`Color _ ) -> `Black
        | `Black   -> `Green
        | `Green   -> `Red
        | `Red     -> `Yellow
        | `Yellow  -> `Blue
        | `Blue    -> `White
        | `White   -> `Black
      in
      let color = !color_state in
      let _ = color_state := color_next color in      
      let skip = 16 in      
      let old_tagid_state = ! tagid_state in 
      
      tagid_state := [] ;
      for i = 0 to 10 do
        for j = 0 to 10 do
          let button_widget = 
            ( Button.create
                ~background:color
                ~anchor:`Nw
                ~text:"R" 
                button_grid ) in
          tagid_state := button :: ! tagid_state ;
          grid ~column:i ~row:j [button]
        done
      done ;

      button_grid.delete canvas old_tagid_state ;
      Timer.remove !timer_state ;
      timer_state := Timer.add ~ms:(!delay_state) ~callback:draw_cb ;
  in
  timer_state := Timer.add ~ms:ms ~callback:draw_cb ;
  ( delay_state, timer_state ) ;;


let channel_gui = Channel_gui.create top ;;
let event_list  = Event_view.List.create top ;;

pack [canvas]                           ~side:`Top ~fill:`Both ~expand:true;;
pack [button_grid]                      ~side:`Top ~fill:`Both ~expand:true;;
pack [channel_gui.Channel_gui.frame]    ~side:`Top ~fill:`X    (*~expand:true*);;
pack [event_list.Event_view.List.frame] ~side:`Top ~fill:`Both (*~expand:true*);;

let somelist = [
  "Hi\tMozart";
  "Bye\tChopin";
  "Beethoven\tGoo";
  "Verdi";
  "Bizet";
  "Mozart";
  "Chopin";
  "Beethoven";
  "Verdi";
  "Bizet";
  "Mozart";
  "Chopin";
  "Beethoven";
  "Verdi";
  "Bizet"]

let _ = Listbox.insert
  ~index:`End
  ~texts:(somelist @ somelist @ somelist @ somelist)
  event_list.Event_view.List.listbox ;;

 ignore ( start_draw_cb 5 ) ;;

let _ = Printexc.print mainLoop ();;
