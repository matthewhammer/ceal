open Hammer_util
open Tv_signal_util
open Tv_trace_zipper
open Tv_surface

module Trace_surface = struct

  module Tz = Trace_zipper
  module S = Surface

  (* Elements on the surface *)
  type elm_desc =
    (* Start and end of the trace *)
    | Start | End
          
    (* Actions *)
    | Action of Action.t
        
    (* Pushes, begin and end, for traces and contexts *)
    | Push_bt | Push_et
    | Push_bc | Push_ec

    (* Marks in the context *)
    | Eval_mark
    | Undo_mark
    | Prop_mark
        
    (* Focal point *)
    | Focus
        
    (* Horizontal lines *)
    | Horz_east
    | Horz_west
        
  module Elm_atts = struct

    (* spatial position of the element, relative to the zipper's focus *)
    type zip_pos = Zp_ctxt | Zp_focus | Zp_trace

    (* temporal position of the element, relative to "now" (i.e., the focus) *)
    type time_pos  = Tp_past | Tp_now | Tp_future | Tp_none

    (* stepping mode to resume with *)
    type resume_mode = Rm_prop | Rm_undo | Rm_none
        
    (* We use these to attribute the focused trace with the
       "intention" of what may happen next (based on what happened
       last) *)
    type last_mode = Lm_prop | Lm_undo | Lm_eval | Lm_none 

    (* Is the element within an enqueued subcomputation? *)
    type queue_pos = Qp_enqueued | Qp_none

    (* Where is the element in relation to the current difference? *)
    type diff_pos = Dp_same | Dp_diff | Dp_only_old | Dp_only_new | Dp_none

    (* All the attributes *)
    type elm_atts = { zip_pos      : zip_pos ;
                      time_pos     : time_pos ;
                      resume_mode  : resume_mode ;
                      last_mode    : last_mode ;
                      action_props : Ap.properties option ;
                      queue_pos    : queue_pos ;
                      diff_pos     : diff_pos ;
                    }

    let diff_pos : Ap.action_diff -> diff_pos = function
      | Ap.Ad_same     -> Dp_same
      | Ap.Ad_diff     -> Dp_diff
      | Ap.Ad_only_old -> Dp_only_old
      | Ap.Ad_only_new -> Dp_only_new
          
    let last_mode : Tz.transition_mode option -> last_mode = function
      | Some Tz.Tm_eval -> Lm_eval
      | Some Tz.Tm_undo -> Lm_undo
      | Some Tz.Tm_prop -> Lm_prop
      | None           -> Lm_none
  end
  open Elm_atts
    
  type elm = {
    elm_desc  : elm_desc ;
    elm_atts  : elm_atts ;
  }

  let elm desc atts = 
    { elm_desc = desc ;
      elm_atts = atts }

  type t = elm S.surf

  module Comb : sig    
    val ctxt   : Ap.query -> elm_atts -> Tz.ctxt  -> t -> t
    val trace  : Ap.query -> elm_atts -> Tz.trace -> t -> t
    val zipper : Tz.transition_mode option -> Ap.query -> Tz.zt -> t -> t
  end
    = 
  struct
    let atts_post_query query a atts = 
      match query a with
        | None   -> atts 
        | Some p -> { atts with diff_pos = diff_pos p.Ap.diff }

    let rec horz elm =
      let rec go = function
        | 0            -> S.id
        | n when n < 0 -> begin S.puts elm **> S.west **> go (n+1) end
        | n when n > 0 -> begin S.puts elm **> S.east **> go (n-1) end
        | _ -> failwith "impossible"
      in go
          
    let rec trace query atts = function
      | Tz.T_end -> 
          begin S.db "T_end" 
            **> S.puts (elm End atts)
          end

      | Tz.T_action (a, t) -> 
          let atts = atts_post_query query a atts in
          begin S.db "T_action"
            **> S.puts (elm (Action a) { atts with action_props = query a } )
            **> S.south 
            **> trace query atts t
          end

      | Tz.T_push (t1, t2) ->
          begin S.db "T_push: begin"
            **> S.puts (elm Push_bt atts)
            **> S.push_cur
            **> S.south
            **> S.db "T_push: t1 begin"
            **> (trace query atts t1) 
            **> S.db "T_push: t1 end"
            **> S.push_cur 
            **> S.op_2_arg 
            (fun ((x',y'),_) ((x,y),_) -> 
               begin S.push_arg ((x+1,y),None)
                 **> S.move_abs
                 **> horz (elm Horz_east atts) (x' - x + 1)
                 **> S.puts (elm Push_et atts)
                 **> S.south
                 **> (trace query atts t2)
               end)
          end

    let rec ctxt query atts = function
      | Tz.C_start -> 
          begin S.db "C_start" 
            **> S.puts (elm Start atts)
          end
            
      | Tz.C_action(c,a) ->
          let atts = atts_post_query query a atts in
          begin S.db "C_action"
            **> S.puts (elm (Action a) { atts with action_props = query a } )
            **> S.north
            **> ctxt query atts c
          end

      | Tz.C_push(c,t) ->
          let t_z  = 
            begin S.db "temp begin"
              **> trace query atts t
              **> S.db "temp end"
            end
          in
          let w, h = S.size (t_z S.empty) in
          begin S.db (Printf.sprintf "C_push: begin (w=%d, h=%d)" w h)
            **> S.puts (elm Push_ec atts)
            **> S.west
            **> S.db "C_push: horz: begin"
            **> horz (elm Horz_west atts) (-w)
            **> S.db "C_push: horz: end"
            **> S.puts (elm Push_bc atts)
            **> S.push_cur
            **> S.south
            **> S.db "C_push: subtrace: begin"
            **> t_z 
            **> S.db "C_push: subtrace: end"
            **> S.move_abs
            **> S.north
            **> S.db "C_push: done"
            **> ctxt query atts c
          end
            
      | Tz.C_eval c ->
          begin S.db "C_eval"
            **> S.puts (elm Eval_mark {atts with time_pos = Tp_none})
            **> S.north
            **> ctxt query atts c
          end
            
      | ( Tz.C_undo (c,t) 
        | Tz.C_prop (c,t) ) as c' 
        ->
          let t_mark, t_atts = match c' with
            | Tz.C_undo _ -> Undo_mark, 
                { atts with 
                    resume_mode = Rm_undo ; 
                    time_pos    = Tp_future }

            | Tz.C_prop _ -> Prop_mark, 
                { atts with 
                    resume_mode = Rm_prop ; 
                    time_pos    = Tp_future }

            | _          -> failwith "impossible"
          in
          begin S.db "C_undo|C_prop: begin"
            **> S.puts (elm t_mark t_atts)
            **> S.push_cur
            **> S.push_cur
            **> S.push_ne
            **> S.db "C_undo|C_prop: begin subtrace"
            **> S.op_2_arg
            (fun ((x_max,_),_) ((x_cur,_),_) ->
               begin S.east
                 **> S.db "C_undo|C_prop: begin horz"
                 **> horz (elm Horz_east t_atts) ((x_max - x_cur) + 1)
                 **> S.db "C_undo|C_prop: end horz"
                 **> S.puts (elm Push_ec t_atts)
                 **> S.south
                 **> trace query t_atts t 
               end)
            **> S.db "C_undo|C_prop: end subtrace"
            **> S.move_abs
            **> S.north
            **> S.db "C_undo|C_prop end"
            **> ctxt query atts c
          end

    let zipper mode query ((c,t),t') =
      let focus_atts = {
        zip_pos      = Zp_focus ;
        time_pos     = Tp_now ;
        resume_mode  = Rm_none ;
        last_mode    = last_mode mode ;
        action_props = None ;
        queue_pos    = Qp_none ;
        diff_pos     = Dp_none ;
      }
      in
      let trace_atts = { 
        focus_atts with
          zip_pos      = Zp_trace ;
          time_pos     = Tp_future ;
          action_props = None ;
      }
      in
      let ctxt_atts = { 
        focus_atts with
          zip_pos      = Zp_ctxt ;
          time_pos     = Tp_past ;
      }
      in
      match t' with
        | None ->
            begin S.db "focus"
              **> S.puts (elm Focus focus_atts)
              **> S.push_cur
              **> ( S.south
                    **> S.db "trace: begin"
                    **> trace query trace_atts t
                    **> S.db "trace: end" )
              **> S.move_abs
              **> ( S.north
                    **> S.db "context: begin"
                    **> ctxt query ctxt_atts c
                    **> S.db "context: end" )
            end
        | Some t' -> 
            begin S.db "focus"
              **> S.puts (elm Focus focus_atts)
              **> S.push_cur
              **> ( S.south
                    **> S.db "trace: begin"
                    **> trace query trace_atts t'
                    **> S.db "trace: end" )
              **> S.move_abs
              **> ( S.north
                    **> S.db "context: begin"
                    **> ctxt query ctxt_atts c
                    **> S.db "context: end" )
            end            
  end
  include Comb


  module Pretty = struct
    open Tv_pretty
    open Tv_pretty.Print
    open Trace_zipper.Pretty

    let elm_desc = function
      | Action a   -> three_chars_of_action a
      | Start      -> " ^ "
      | End        -> " $ "
      | Push_bt    -> "psh"
      | Push_et    -> "-. "
      | Push_bc    -> "psh"
      | Push_ec    -> "-. "
      | Eval_mark  -> "[ ]"
      | Undo_mark  -> "[-]"
      | Prop_mark  -> "[+]"
      | Focus      -> " o "
      | Horz_east  -> "---"
      | Horz_west  -> "---"

    let elm e = 
      match 
        e.elm_desc,
        e.elm_atts.zip_pos, 
        e.elm_atts.time_pos, 
        e.elm_atts.resume_mode,
        e.elm_atts.last_mode,
        e.elm_atts.diff_pos
      with                    
        (* Undo-mode highlighting *)
        | _, Zp_trace, Tp_future, _      , Lm_undo, _     -> bright (red   (elm_desc e.elm_desc))
        | _, Zp_ctxt,  Tp_future, Rm_undo, Lm_undo, _     -> bright (red   (elm_desc e.elm_desc))
        | _, Zp_ctxt,  Tp_future, Rm_undo, _      , _     ->        (red   (elm_desc e.elm_desc))

        (* Prop-mode highlighting *)
        | _, Zp_ctxt,  Tp_future, Rm_prop, _, _           -> blue   (elm_desc e.elm_desc)
        | _, Zp_trace, Tp_future, _      , _, _           -> cyan   (elm_desc e.elm_desc)

        (* Highlight horizontal lines subtley *)
        | (Horz_east|Horz_west), _, _, _, _, Dp_only_new -> (white (elm_desc e.elm_desc))
        | (Horz_east|Horz_west), _, _, _, _, Dp_diff     -> (white (elm_desc e.elm_desc))
        | (Horz_east|Horz_west), _, _, _, _, _           -> (blue (elm_desc e.elm_desc))
            
        (* Highlight the focus *)
        | _, Zp_focus, _, _, Lm_undo, _          -> (red_b   (black (elm_desc e.elm_desc)))
        | _, Zp_focus, _, _, Lm_eval, _          -> (green_b (black (elm_desc e.elm_desc)))
        | _, Zp_focus, _, _, Lm_prop, _          -> (magenta_b (black (elm_desc e.elm_desc)))
        | _, Zp_focus, _, _, Lm_none, _          -> (cyan_b  (black (elm_desc e.elm_desc)))

        (* Highlight things in the past *)
        | _, Zp_ctxt,  Tp_past,   _, _    , Dp_only_new -> white_b (blue (elm_desc e.elm_desc))
        | _, Zp_ctxt,  Tp_past,   _, _    , Dp_diff     -> white_b (blue (elm_desc e.elm_desc))
        | _, Zp_ctxt,  Tp_past,   _, _    , _           -> blue_b  (black (elm_desc e.elm_desc))

        | _                                             -> elm_desc e.elm_desc

    let vt100 surf (w,h) lb le = 
      let ne  = (w/2 - (if w mod 2 = 0 then 1 else 0),
                 h/2 - (if h mod 2 = 0 then 1 else 0)) in 
      let sw  = (-w/2,-h/2) in
      let lines = S.as_list ne sw surf in
      let buff = Buffer.create 0 in
      List.iter begin fun row ->
        Buffer.add_string buff lb ;
        List.iter begin function
          | (x,y), Some e -> Buffer.add_string buff (elm e)
          | (x,y), None   -> 
              if x mod 4 = 0 && y mod 4 == 0 then
                Buffer.add_string buff (magenta " . ")
              else
                Buffer.add_string buff ("   ")
        end row ;
        Buffer.add_string buff le ;
      end lines ;
      buff
        
    let ott surf = 
      raise NYI

  end

end
