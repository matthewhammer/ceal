open Hammer_util
open Tv_signal_util
open Tv_trace_zipper

module MIRROR_IO 
  ( Control : MONAD )
  = 
struct
  module type S = sig
    
    val read_event : Tv_signal.event Control.m
    val read_tags  : Channel.tag list Control.m

    type phase = [ `Begin | `Interm | `End ]

    val forw : phase -> (unit Control.m) Control.m
    val back : phase -> unit Control.m -> unit Control.m

    val set_action_diff  : Action.t -> Ap.action_diff option -> unit Control.m
    val set_action_state : Action.t -> Ap.action_state option -> unit Control.m
    val set_action_props : Action.t -> Ap.properties option -> unit Control.m
  end
end

module MIRROR_CONTROL
  ( Control  : MONAD )
  = 
struct
  module type S = sig
    type pos
    type control = pos -> pos Control.m

    val start : pos
    val forw : control
    val back : control
    val jump : pos -> control
  end
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

module Mirror_control
  ( C  : MONAD (* from GUI side. *))
  ( Io : MIRROR_IO(C).S (* from GUI side. *))
  ( Tr : TRACE_REP(C).S (* from GUI side. *))
  : MIRROR_CONTROL(C).S
  =
struct

  module Actions = Set.Make(Action_keyelt)
  module Mi = Map.Make(String)

  type state = {
    fsta : Action.id Mi.t ; (* maps trnode_ids to their first actions *)
    acts : Actions.t ; (* set of currently-existing actions *)
    
    doit : (unit C.m) C.m ;
    undo : unit C.m ;
    
    event : event option ;
    tags  : Tagset.t ;
  }
      
  (* Bistream controls *)
  module Comb : sig
    val invoke : Action.t            -> unit C.m
    val revinv : Action.t            -> unit C.m
    val revoke : Action.id           -> unit C.m
    val reuse  : Tv_signal.trnode_id -> unit C.m
    val redo   : Tv_signal.trnode_id -> unit C.m
    val propto : Tv_signal.trnode_id -> unit C.m   
  end = struct
    (* .... *)
  end

  type pos = {
    past : state list ;
    curr : state ;
    futr : state list ;
  }


  let start = 
    let start_state = {
      doit = C.ret () ;
      undo = C.ret () ;
    }
    in { 
      past = [] ;
      curr = start_state ;
      futr = [] ;
    }

  let rec forw_fresh : (unit C.m) C.m = 
    Io.read_tags >>= fun tags ->
    Io.read_event >>= fun event ->
    

    Io.forw `Begin >>= fun undo_begin ->
    

    Io.forw `End >>= fun undo_end ->
      ( undo_begin >>>
          undo_end )

  let forw pos =
    match pos.futr with
      | state :: futr -> {
          past = past :: 
        }

      | [] -> 



end

module Mirror : sig 
  
  type bistream
  type control = bistream -> bistream

  type get_event = unit -> Tv_signal.event option
  
  type move_phase = [ `Begin | `Step | `End ]

  type callback =
      [ `Forward   of move_phase * Channel.Kind_set.t
      | `Backward  of move_phase * Channel.Kind_set.t
      | `Get_event of Tv_signal.event
      ] -> control

  type fns = {
    get_event : get_event ;
    callback  : callback option ;
  }
  
  module State_id : sig 
    type t 
    val compare : t -> t -> int 
  end
      
  val create    : fns -> bistream

  val forward      : Channel.Kind_set.t -> control
  val backward     : Channel.Kind_set.t -> control
  val set_callback : callback option    -> control

  type state
      
  val get_state        : bistream -> state
  val get_id           : state -> State_id.t
  
  val get_action_diff  : state -> Action.t -> Ap.action_diff option
  val get_action_state : state -> Action.t -> Ap.action_state option
  val get_action_props : state -> Action.t -> Ap.properties option
  
  val get_fst_action   : state -> Tv_signal.trnode_id -> Action.id
  val get_tz           : state -> Trace_zipper.zt
  val get_last_mode    : state -> Trace_zipper.transition_mode option

  type event = Tv_signal.event
  
  val get_events       : int -> bistream -> event list * event list
    
end = struct

  module Ma = Map.Make(Action_keyelt)
  module Mi = Map.Make(String)
  module Tagset = Channel.Tag_set

  open Ap

  type event = Tv_signal.event

  module State_id = struct 
    type t = Initial | Succ of int * t
    let compare = compare
  end

  type state = { 
    id     : State_id.t ;
    tz     : Trace_zipper.zt ;
    states : action_state Ma.t ;
    diffs  : action_diff Ma.t ;
    decls  : Tv_signal.decl Mi.t ; (* see Tv_signal.decl *)
    fsta   : Action.id Mi.t ; (* maps trnode_ids to their first actions *)
    curra  : Action.id ;
    mode   : Trace_zipper.transition_mode option ;
    event  : event option ;
    tags   : Tagset.t ; (* a set of channel tags *)
    fresh  : bool ; 
  }
    
  type bistream = {
    past   : state list ;
    curr   : state ;
    future : state list ;
    fns    : fns ;
  }

  and control = bistream -> bistream

  and get_event = unit -> Tv_signal.event option
  and callback =
      [ `Forward 
      | `Backward
      | `Get_event of Tv_signal.event
      ] -> control

  and fns = {
    get_event : get_event ;
    callback  : callback option ;
  }
  
  module Tz_out = struct
    type s = bistream
    type t = control
    type q = Trace_zipper.zt
    
    let none s = s

    let next comb s = 
      { s with 
          past = s.curr :: s.past ;
          curr = { s.curr with
                     tags  = Tagset.empty ;
                     event = None ;
                     fresh = true ; 
                     tz = comb s.curr.tz } }

    let guard g s = ( g s.curr.tz ) s

    let peek f s = ignore (f s.curr.tz) ; s
        
    let set_tags tags s =
      { s with curr = 
          { s.curr with tags = 
              List.fold_right Tagset.add tags Tagset.empty  } }

    let add_tags tags s =
      { s with curr = 
          { s.curr with tags = 
              List.fold_right Tagset.add tags s.curr.tags } }
  end

  module Tz = struct
    include Trace_zipper
    include Trace_zipper.Comb ( Tz_out )
  end

  let initial_state = {
    id     = State_id.Initial ;
    tz     = Tz.empty ;
    states = Ma.empty ;
    diffs  = Ma.empty ;
    decls  = Mi.empty ; 
    fsta   = Mi.empty ;
    curra  = Action.id' Action.the_start ;
    mode   = None ;
    event  = None ;
    tags   = Tagset.empty ;
    fresh  = false ;
  }

  (* Bistream controls *)
  module Comb : sig
    val none   : control
    val invoke : Action.t            -> control
    val revinv : Action.t            -> control
    val revoke : Action.id           -> control
    val reuse  : Tv_signal.trnode_id -> control
    val redo   : Tv_signal.trnode_id -> control
    val propto : Tv_signal.trnode_id -> control    
    val diffs_clear : control
  end 
    = 
  struct
    let none s = s
      
    let invoke a s = 
      let s = 
        Tz.eval a s 
      in
      { s with curr = 
          { s.curr with
              diffs  = Ma.add (Action.id' a) Ad_only_new s.curr.diffs ;
              states = Ma.add (Action.id' a) As_allocated s.curr.states ; 
              curra  = Action.id' a ;
              fsta = ( let id = Action.id' a in 
                       if Mi.mem (fst id) s.curr.fsta then s.curr.fsta 
                       else Mi.add (fst id) id s.curr.fsta ) ;
              mode = Some Tz.Tm_eval ;
          } }
        
    let revinv a s = 
      let s = 
        begin Tz.undoto (Action.id' a) 
          **> Tz.undo
          **> Tz.eval a 
        end s
      in
      { s with curr =
          { s.curr with
              diffs = Ma.add (Action.id' a) Ad_diff s.curr.diffs ;
              curra = Action.id' a ;
              mode = Some Tz.Tm_eval ;
          } }
    
    let revoke i s = 
      let s = 
        begin Tz.undoto i 
          **> Tz.undo
        end s
      in
      { s with curr =
          { s.curr with
              diffs  = Ma.add i Ad_only_old s.curr.diffs ;
              states = Ma.add i As_collected s.curr.states ;
              fsta   = ( if Mi.mem (fst i) s.curr.fsta then 
                           Mi.remove (fst i) s.curr.fsta 
                         else s.curr.fsta ) ;
              mode = Some Tz.Tm_undo ;
          } }

    let redo trnode_id s = 
      let i = (Mi.find trnode_id s.curr.fsta) in
      let s =
        begin Tz.propto i 
          **> Tz.prop_beyond_update
        end s
      in
      { s with curr = 
          { s.curr with
              curra  = i ;
              mode   = Some Tz.Tm_eval ;
          } }
    
    let reuse trnode_id s = 
      let i = (Mi.find trnode_id s.curr.fsta) in
      let s = Tz.undoto i s 
      in
      { s with curr =
          { s.curr with
              curra  = i ;
              mode   = Some Tz.Tm_prop ;
          } }

    let propto t s = 
      let i = (Mi.find t s.curr.fsta) in
      let s = 
        begin Tz.propto i 
          **> Tz.prop_beyond_trnode
        end s
      in
      { s with curr =
          { s.curr with
              curra  = i ;
              mode = Some Tz.Tm_prop ;
          } }
                    
    let diffs_clear : control = fun s -> 
      { s with curr = 
          { s.curr with
              diffs = ( Ma.fold (fun i _ diffs -> Ma.add i Ad_same diffs) 
                          s.curr.diffs Ma.empty ) 
          } }
  end

    
  (* Handle a step, transform bistream. See: Tv_signal.step *)
  let step : Tv_signal.step -> control = fun step ->
    try (
      match step with
        | `S_invoke a -> Comb.invoke a (* invoke, eval a *)
        | `S_revinv a -> Comb.revinv a (* revinv, undoto id(a) + eval a *)
        | `S_revoke i -> Comb.revoke i (* revoke, undoto i *)
        | `S_reuse  t -> Comb.reuse  t (* reuse, E.P. *)
        | `S_redo   t -> Comb.redo   t (* redo,  P.E. *)
        | `S_propto t -> Comb.propto t (* propto *)
    ) 
    with 
      | Tz.Illegal_transition (zt,name) ->
          Printf.printf "Error: Illegal transition: %s" name
          ;
          Printf.printf "Last step: %s\n"
            (Tv_signal.string_of_step step)
          ;
          Printf.printf "\n==Zipper==\n%s\n" (Tz.Pretty.zipper zt)
          ;
          exit(1)

  (* Handle a decl, transform bistream. See: Tv_signal.decl *)  
  let decl : Tv_signal.decl -> control =
    fun decl s -> {
      s with curr = {
        s.curr with decls = Mi.add (trnode_id_of_decl decl) decl s.curr.decls
      } }

  (* Handle a meta-level step, transform bistream. See: Tv_signal.meta *)
  let meta : Tv_signal.meta -> control = function
    | (_, `M_core_begin) -> Comb.diffs_clear
    | (_, `M_core_end)   -> Tz.rewind_full
    | (_, `M_prop_begin) -> Comb.diffs_clear
    | (_, `M_prop_end)   -> Tz.rewind_full
    | _ -> (* TODO *) Tz_out.none

  (* Handle an event, transform bistream. See: Tv_signal.event *) 
  let event : Tv_signal.event -> control = 
    let set_event e s = 
      { s with curr = { s.curr with event = Some e } }
    in
    fun event -> match event with
      | `E_step s -> set_event event **> step s
      | `E_decl d -> set_event event **> decl d
      | `E_meta m -> set_event event **> meta m

  (* - - - Main Controls - - - - *)
        
  let create fns = {
    past   = [] ;
    curr   = initial_state ;
    future = [] ;
    fns    = fns }

  exception No_more

  let callback param s =
    match s.fns.callback with 
      | Some cb -> cb param s 
      | None    -> s

  let rec forward kinds s =
    let rec return s = 
      match s.past with
        | [] -> s
        | state :: _ when state.fresh = false -> s
        | state :: past -> 
            return { s with 
                       past   = past ;
                       curr   = state ; 
                       future = s.curr :: s.future }
    in 
    let unfreshen state = { state with fresh = false } in
    let next s = 
      match s.future with
        | state :: future ->
            { s with
                past = (unfreshen state) :: s.past ;
                curr = state ;
                future = future ;
            }
        | [] -> (
            match s.fns.get_event () with
              | None    -> raise No_more
              | Some ev -> 
                  return ( event ev 
                             ( callback (`Get_event ev) 
                                 { s with curr = unfreshen s.curr } ) )
          )
    in
    try
      let s = next (callback (`Forward (`Begin, kinds))) s in
      if Channel.match_tags s.curr.tags kinds 
      then callback (`Forward (`End, kinds)) s
      else forward kinds (callback (`Forward (`Step, kinds))) s
    with
      | No_more -> s

  let rec backward kinds s =
    let next s =
      match s.past with
        | state::past ->
            { s with
                curr   = state ;
                future = s.curr :: s.future }
        | [] -> raise No_more
    in
    try
      let s = next s in
      if Channel.match_tags s.curr.tags kinds 
      then callback `Backward s
      else backward kinds s
    with
      | No_more -> s

  let set_callback cb bistream =
    { bistream with fns = { bistream.fns with callback = cb } }

  (* - - - Getters for state - - - - *)

  let get_state s = s.curr

  let get_id s = s.id

  let get_action_diff s a = 
    try Some (Ma.find (Action.id' a) s.diffs)
    with Not_found -> None

  let get_action_state s a = 
    try Some(Ma.find (Action.id' a) s.states) 
    with Not_found -> None

  let get_action_props s a = try Some {
    Ap.state = Ma.find (Action.id' a) s.states ;
    Ap.diff  = Ma.find (Action.id' a) s.diffs  }
  with Not_found -> None

  let get_fst_action s trnode_id = Mi.find trnode_id s.fsta

  let get_tz s = s.tz

  let get_last_mode s = s.mode

  let get_events number s =
    let rec getn n ss = match n, ss with
      | 0, ss -> []
      | _, [] -> []
      | _, s :: ss -> 
          ( match s.event with 
              | Some ev -> ev :: getn (n-1) ss
              | None    -> getn n ss
          )
    in
    let past = List.rev (getn (number / 2) s.past) in
    let past_len = List.length past in
    let future = getn (number - past_len) s.future in
    let future_len = List.length future in
    if past_len + future_len = number then
      past, future
    else
      let past = List.rev (getn (number - future_len) s.past) in
      past, future

end
