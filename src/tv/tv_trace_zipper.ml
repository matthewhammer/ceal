(* Matthew Hammer <hammer@mpi-sws.org> *)

open Hammer_util
open Tv_signal_util

(* We use the notion of "tags" to attribute states of the SASM.  Each
   tag gives some semantic information about a state (of the SASM)
   that we use as meta-data; this we filter on and search within this
   meta-data during an interactive session, which allows us to filter
   and search for the corresponded states. *)
module TAGS ( Control : MONAD ) = struct
  module type S = sig
    val set : Channel.tag list -> unit Control.m
    val add : Channel.tag list -> unit Control.m
    val get : Channel.tag list Control.m
  end
end

(* signature WRAPABLE.S gives an ADT for a GUI widget when this widget
   represents the constructor of an inductive data type (of type raw);
   after introduction, we wrap; before elimination, we unwrap.  More
   specific instances of this interface appear below in TRACE_REP.
*)
module WRAPABLE ( Control : MONAD ) = struct
  module type S = sig
    type raw
    type wrapped
      
    val wrap   : raw -> wrapped Control.m (* create wrapping. *)
    val unwrap : wrapped -> raw Control.m (* discard wrapping. *)
    val peek   : wrapped -> raw    (* peek inside of wrapping. *)
  end
end

(* TRACE_REP gives an abstract trace representation, as a "translucent
   sum".  Note that while we expose the constructors for types trace
   and ctxt, both of these types are defined in terms of wrapped
   versions trace_wrapped and ctxt_wrapped, respectively; morover, we
   do not expose the constructors for any wrapped forms.  These
   wrapped forms can only be created within the control monad.
   Similarly, the wrapped forms can only be unwrapped within the
   control monad.  This captures the fact that creating and destroying
   widgets is an effectful operation, and that we will do something
   special with these effects (namely, make them reversible).
*)
module TRACE_REP ( Control : MONAD )
  = 
struct
  module type S = sig
    type action_wrapped (* "linear" -- has unique "parent value". *)
    type action
      
    type trace_wrapped (* "linear" -- has unique "parent value". *)
    type trace =
      | T_end
      | T_action of action_wrapped * trace_wrapped
      | T_push   of trace_wrapped  * trace_wrapped
          
    type ctxt_wrapped (* "linear" -- has unique "parent value". *)
    type ctxt = 
      | C_start
      | C_action of ctxt_wrapped * action_wrapped
      | C_push   of ctxt_wrapped * trace_wrapped
      | C_eval   of ctxt_wrapped
      | C_undo   of ctxt_wrapped * trace_wrapped
      | C_prop   of ctxt_wrapped * trace_wrapped
          
    type z     = ctxt_wrapped * trace_wrapped
    type z_raw = ctxt * trace
        
    type zt     = z     * trace_wrapped option
    type zt_raw = z_raw * trace option
        
    val empty : zt
          
    (* Getters and settings for the SASM zipper. *)
    val get  : zt Control.m (* get the SASM trace. *)
    val set  : zt -> unit Control.m (* overwrite the SASM trace. *)
    val step : unit Control.m (* start a distinct view of the SASM. *)
      
    module Action : WRAPABLE(Control).S
      with type raw     = action
      and  type wrapped = action_wrapped
                    
    module Trace : WRAPABLE(Control).S
      with type raw     = trace 
      and  type wrapped = trace_wrapped
      
    module Ctxt : WRAPABLE(Control).S
      with type raw     = ctxt
      and  type wrapped = ctxt_wrapped
      
    module Z : WRAPABLE(Control).S
      with type raw     = z_raw
      and  type wrapped = z
      
    module Zt : WRAPABLE(Control).S
      with type raw     = zt_raw
      and  type wrapped = zt
  end
end

(* Self-adjusting stack machine controls. *)      
module SASM_CONTROL 
  ( Control : MONAD )
  ( Trace_rep : TRACE_REP(Control).S ) =
struct
  module type S = sig
        
    val eval   : Trace_rep.action -> unit Control.m
    val undo   : unit Control.m
    val prop   : unit Control.m
    
    val undoto : Action.id -> unit Control.m
    val propto : Action.id -> unit Control.m
      
    val prop_beyond_update : unit Control.m
    val prop_beyond_trnode : unit Control.m
      
    val rewind      : unit Control.m
    val rewind_full : unit Control.m    
  end
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* SASM_CONTROL implementation. *)
module Sasm_control
  ( C    : MONAD (* from GUI side. *))
  ( Tr   : TRACE_REP(C).S with type action = Action.t (* from GUI side. *))
  ( Tags : TAGS(C).S (* not from GUI? *))
  : SASM_CONTROL(C)(Tr).S =
struct
  type control = unit C.m
  include Monad_shorthands (C)  
  
  module Pretty : sig
    (* We use this module for doing simple pretty-printing, which
       enables the debugging support that we used to debug the
       combinators defined below. *)

    val focus     : string
    val eval_mark : string
    val prop_mark : string
    val undo_mark : string

    val trace     : Tr.trace  -> string
    val ctxt      : Tr.ctxt   -> string
    val zipper    : Tr.zt_raw -> string
  end 
    = 
  struct
    open Tv_pretty
    open Tv_pretty.Print

    let blue   = blue
    let bright = bright
    let red    = red

    let focus     = red " o "
    let eval_mark = bright (blue "[ ]")
    let undo_mark = bright (blue "[-]")
    let prop_mark = bright (blue "[+]")

    let rec trace = function
      | Tr.T_end          -> "$"
      | Tr.T_action (a,t) -> (chars_of_action (Tr.Action.peek a))^"."^(trace (Tr.Trace.peek t))
      | Tr.T_push (t1,t2) -> "( "^(trace (Tr.Trace.peek t1))^" )."^(trace (Tr.Trace.peek t2))
          
    let rec ctxt = function
      | Tr.C_start        -> "^"
      | Tr.C_action (c,a) -> (ctxt (Tr.Ctxt.peek c))^"."^(chars_of_action (Tr.Action.peek a))
      | Tr.C_push(c,t)    -> (ctxt (Tr.Ctxt.peek c))^"."^(blue "( ")^(trace (Tr.Trace.peek t))^(blue " )")
      | Tr.C_eval c       -> (ctxt (Tr.Ctxt.peek c))^"."^eval_mark
      | Tr.C_undo(c,t)    -> (ctxt (Tr.Ctxt.peek c))^"."^undo_mark^(blue ("-->( "^(trace (Tr.Trace.peek t))^" )"))
      | Tr.C_prop(c,t)    -> (ctxt (Tr.Ctxt.peek c))^"."^prop_mark^(blue ("-->( "^(trace (Tr.Trace.peek t))^" )"))

    let zipper ((c,t),_) = (dim (ctxt c)) ^ focus ^ (bright (trace t))
  end

  let debug_flag = ref false
    
  exception Illegal_transition of Tr.zt * string
    
  exception Impossible

  module Ch = Channel
    
  let empty = ((Tr.C_start, Tr.T_end), None)
    
  (* - - - - trace predicates - - - - - *)

  let rec is_end : Tr.trace -> bool =
    function
      | Tr.T_end           -> true
      | Tr.T_action (wrapped, t) -> (
         let (_,_,_,d) = Tr.Action.peek wrapped in
         is_end (Tr.Trace.peek t)
         && match d with 
           | `A_end -> true 
           | otherwise -> false
        )
      | otherwise -> false
          
  let trace_has_head_with_id i = function
    | Tr.T_end           -> false
    | Tr.T_push _        -> false
    | Tr.T_action(aw, t) -> ( 
        match Action.id (Tr.Action.peek aw) with 
          | Some i' when i = i' -> true 
          | _                   -> false
      )

  let trace_has_orphaned_head = function
    | Tr.T_end          -> false
    | Tr.T_push _       -> false
    | Tr.T_action(a, t) -> Action.has_nil_trnode (Tr.Action.peek a)

  let trace_has_update_head = function
    | Tr.T_action (aw, t) -> 
        let (_,_,_,desc) = Tr.Action.peek aw in
        ( match desc with 
            | `A_update _ -> true 
            | _           -> false )
    | _ -> false
        
  let rec traces_have_same_trnode : Tr.trace -> Tr.trace -> bool = 
    let trnode_id = function
      | Tr.T_push(t1, t2)  -> None
      | Tr.T_end           -> None
      | Tr.T_action(a, t)  -> Some (Action.trnode_of (Tr.Action.peek a))
    in
    ( fun t1 t2 -> 
        trnode_id t1 = trnode_id t2 
        && 
        trnode_id t1 <> None )

  (* - - - - - Debugging - - - - - *)

  let db str prepost : unit C.m = Tr.get >>= fun ((c,t),t') -> 
    let action_string = match Tr.Trace.peek t with 
      | Tr.T_action(a, _) -> (Tv_signal.string_of_action (Tr.Action.peek a))
      | _              -> "(no action in focus)"
    in
    let prepost_string = match prepost with 
      | `Pre  -> "pre" 
      | `Post -> "post"
    in
    if ! debug_flag then (
      Printf.printf "-- %40s: (%4s): %s\n" str prepost_string action_string 
      ;
      Printf.printf "-- %40s:       : %s\n" str (Pretty.zipper (Tr.Zt.peek ((c,t),t')))
    ) ; C.ret ()

  (* - - - - - Rewinding - - - - - *)

  (* Rewind 1 step. *)

  let rewind1 : control = db "rewind1" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_rewind_step Ch.Pre] >>>
    Tr.step >>>
    Tr.get >>= begin function
      | (c,t), None -> (
          Tr.Trace.wrap Tr.T_end >>= fun tw -> 
            C.set ((c,t), Some tw) >>> C.ret ()
        )
          
      | (c,t1), Some t3 -> Tr.Ctxt.unwrap c >>= function
          | Tr.C_action(c, a) -> (
              Tr.Trace.wrap (Tr.T_action(a, t3)) >>= fun t3' ->
                C.ret ((c,t1), Some t3') >>> C.ret ()
            )
              
          | Tr.C_push(c, t2) -> (
                Tr.Trace.wrap (Tr.T_push(t2, t3)) >>= fun t3' ->
                  C.set ((c,t1), Some t3') >>> C.ret ()
            )
              
          | Tr.C_undo(c, t2) when Tr.Trace.peek t1 = Tr.T_end -> (
              C.set ((c, t2), Some t3) >>> C.ret ()
            )
              
          | Tr.C_undo(c, t2) -> ( 
              Tr.Trace.wrap (Tr.T_push(t1, t2)) >>= fun t1' ->
                C.set ((c, t1'), Some t3) >>> C.ret ()
            )
              
          | c -> (
              Tr.Ctxt.wrap c >>= fun c ->
                C.set ((c, t1), Some t3) >>> C.ret ()
            )
    end >>>
    db "rewind1" `Post >>>
    Tags.set [Ch.tag Ch.Sasm_rewind_step Ch.Post]
      
  (* Rewind as far as possible. *)
   let rewind : control =
    let rec loop () = Tr.get >>= fun zt -> match Tr.Zt.peek zt with
        (* More rewinding... *)
      | ( (Tr.C_action(_, _),   _ ),  _
        | (Tr.C_push(_, _),     _ ),  _
        | (Tr.C_undo(_, _), Tr.T_end ),  _ 
        | (Tr.C_undo(_, _),     _ ),  _ ) -> rewind1 >>> loop ()
          (* Done rewinding *)
      | otherwise -> C.ret () 
    in loop ()

  (* Do the rewind fully to the start of the trace. *)
  let rewind_full : control = db "rewind_full" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_rewind_full Ch.Pre] >>>
    rewind >>>
    Tr.get >>= fun (((c,_),_) as zt) -> match Tr.Zt.peek zt with
      | (Tr.C_start, tend), Some t when is_end tend -> (
          Tr.Trace.wrap t >>= fun t ->
          Tr.set ((c, t), None) >>>
          Tags.set [Ch.tag Ch.Sasm_rewind_full Ch.Post] >>>
          db "rewind_full" `Post
        )
      | zt -> 
          Printf.printf "%s: final rewind yielded:\n%s\n"
            (Tv_pretty.red "Error")
            (Pretty.zipper zt)
          ; exit(1)

  (* Do the "rewind phase" of popping the stack. *)
  let rewind_pop : control = db "rewind_pop" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_rewind_pop Ch.Pre] >>>
    rewind >>>
    Tr.get >>= begin function 
      | ((c0, t1), Some t3) -> Tr.Ctxt.unwrap c0 >>= (
          function
            | Tr.C_eval c' -> (
                Tr.Ctxt.wrap (Tr.C_push(c', t3)) >>= fun c'' ->
                  Tr.set ((c'', t1), None)
              )
                
            | Tr.C_prop (c',t2) when is_end (Tr.Trace.peek t1) -> (                  
                Tr.Ctxt.wrap (Tr.C_push(c', t3)) >>= fun c'' ->
                  Tr.set ((c'', t2), None)
              )
                
            | Tr.C_start when is_end (Tr.Trace.peek t1) -> (
                Tr.Ctxt.wrap Tr.C_start >>= fun c'' ->
                  Tr.set ((c'', t3), None)
              )
                
            | _ -> raise (Illegal_transition (((c0,t1), Some t3), "pop_rewind" ))
        )
          
      | (z, None) -> raise (Illegal_transition ((z, None), "pop_rewind"))
    end >>>
    Tags.set [Ch.tag Ch.Sasm_rewind_pop Ch.Post] >>>
    db "rewind_pop" `Post

  (* - - - - - Evaluation - - - - - *)
      
  let rec eval ((_,_,_,desc) as a) : control = db "eval" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_eval_step Ch.Pre] >>>
    Tr.step >>>
    Tr.get >>= (
      function
        | ((_,_), Some _) as zt -> raise (Illegal_transition (zt,"eval"))
        | (c,t), None -> ( 
            match desc with
              | `A_push_begin -> ( 
                  Tr.Ctxt.wrap (Tr.C_eval c) >>= fun c' ->
                    Tr.set ((c', t), None)
                )
              | `A_push_end   -> rewind_pop
              | _             -> (
                  Tr.Action.wrap a >>= fun a ->
                  Tr.Ctxt.wrap (Tr.C_action(c, a)) >>= fun c' ->
                  Tr.set ((c', t), None)
                )
          )
    ) >>>
    Tags.set [Ch.tag Ch.Sasm_eval_step Ch.Post] >>>
    db "eval" `Post

  (* - - - - - Propagation - - - - - *)

  let rec prop1 : control = db "prop1" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_prop_step Ch.Pre] >>>
    Tr.step >>>
    Tr.get >>= (
      function
        | ((_,_), Some _) as zt -> raise (Illegal_transition (zt,"prop1"))
        | (c,t), None -> ( 
            Tr.Trace.unwrap t >>= function
              | Tr.T_push(t1, t2) -> (
                  Tr.Ctxt.wrap (Tr.C_prop(c, t2)) >>= fun c' ->
                  Tr.set ((c', t1), None)
                )
              | Tr.T_action(a, t) -> (
                  Tr.Ctxt.wrap (Tr.C_action(c, a)) >>= fun c' ->
                  Tr.set ((c', t), None)
                )
              | Tr.T_end -> rewind_pop
          )
    ) >>>
    Tags.set [Ch.tag Ch.Sasm_prop_step Ch.Post] >>>
    db "prop1" `Pre

  let rec prop_while guard_str (guard : Tr.trace -> bool) : control =
    db ("prop_while: " ^ guard_str) ` Pre >>>
    Tr.get >>= fun ((_,t),_) -> 
    if guard (Tr.Trace.peek t) then 
      prop1 >>> prop_while guard_str guard
    else
      db ("prop_while: " ^ guard_str) ` Post
      
  let rec prop : control = db "prop" `Pre >>>
    prop1 >>>
    prop_while "orphans" trace_has_orphaned_head >>>
    db "prop" `Post

  let propto i : control = 
    let is = Tv_pretty.string_of_action_id i in
    db (Printf.sprintf "propto %s" is) `Pre >>>
    let rec loop () = 
      Tr.get >>= fun ((_,t),_) -> 
        if trace_has_head_with_id i (Tr.Trace.peek t) then 
          db (Printf.sprintf "propto %s" is) `Post
        else prop >>> loop () 
    in
    loop ()

  let prop_beyond_update = db "prop_beyond_update" `Pre >>>
    let rec loop () = 
      Tr.get >>= fun ((_,t),_) -> 
        if trace_has_update_head (Tr.Trace.peek t) then 
          prop >>> db "prop_beyond_update" `Post
        else 
          prop >>> loop ()
    in 
    loop ()

  let rec prop_beyond_trnode = 
    Tr.get >>= fun ((_,t),_) ->
      let guard : Tr.trace -> bool = fun t' ->
        ( not ( trace_has_update_head t' ) 
          && ( traces_have_same_trnode (Tr.Trace.peek t) t'
               || trace_has_orphaned_head t' ) )
      in
      ( prop_while "same_trnode" guard )

  (* - - - - - Undoing - - - - - *)

  let rec undo1 = db "undo1" `Pre >>>
    Tags.add [Ch.tag Ch.Sasm_undo_step Ch.Pre] >>>
    Tr.step >>>
    Tr.get >>= begin function
      | ((_,_), Some _) as zt -> raise (Illegal_transition (zt, "undo1"))
      | ((c,t), None  ) as zt -> (
          match (Tr.Ctxt.peek c, Tr.Trace.peek t) with
            | (_, Tr.T_action(_, t)) -> 
                Tr.set ((c, t), None)
                
            | (_, Tr.T_push(t1, t2)) -> (
                Tr.Ctxt.wrap (Tr.C_undo(c, t2)) >>= fun c' ->
                Tr.set ((c', t1), None)
              )
                
            | (Tr.C_undo(c, t2), Tr.T_end) -> 
                Tr.set ((c, t2), None)
                
            | _ -> raise (Illegal_transition (zt, "undo1"))
        ) end >>>
    Tags.set [Ch.tag Ch.Sasm_undo_step Ch.Post] >>>
    db "undo1" `Post

  let undo_orphaned = db "undo_orphaned" `Pre >>>
    let rec loop () = 
      Tr.get >>= fun ((_,t),_) ->
        if trace_has_orphaned_head (Tr.Trace.peek t) then 
          undo1 >>> loop ()
        else 
          db "undo_orphaned" `Post
    in loop ()

  let undo = db "undo" `Pre >>>
    undo1 >>>
    undo_orphaned >>>
    db "undo" `Post

  let undoto i =
    let is = Tv_pretty.string_of_action_id i in
    db (Printf.sprintf "undoto %s" is) `Pre >>>
    let rec loop () = Tr.get >>= fun ((_,t),_) ->
      if trace_has_head_with_id i (Tr.Trace.peek t) then 
        db (Printf.sprintf "undoto %s" is) `Post
      else 
        undo >>> loop ()
    in
    loop ()      

end
