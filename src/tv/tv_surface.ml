(* Matthew Hammer <hammer@mpi-sws.org> *)

(* The metaphor is that this Surface module provides surface-like
   objects like those found in drawing libraries such as SDL; (that
   is, where each object is a buffer full of pixels).  In our case, we
   do not specify what sort of "stuff" goes into the surface---we are
   parametric in this.  

   Coordinate system -- The surface has its own coordinate system in
   2D space, oriented around (0,0).  This fact only becomes important
   when using absolute coordinates, which are not always neccesary in
   client code.

   Focus movement --- The focus can move in cardinal directions
   (north, south, east, west) by integer intervals.  The current
   absolute position can be pushed onto the stack (see explanation
   below) for tracing tree-like movement on the grid, i.e., by saving
   the position, moving around and restoring the saved position later.

   In the focused position, the parametric content can be set and
   cleared in arbitrary client-specified ways.

   Argument stack --- All commands consume their parameters from an
   explicit argument stack (note, this is not a control stack---it
   holds arguments, not return continuations).
*)

open Hammer_util

module Surface = struct

  let debug_flag = false
  
  type xy = int * int
      
  (* stack element = operation arg. *)
  type 'a arg = xy * 'a option

  type 'a row = { 
    east  : 'a option list ;
    x     : int ; 
    here  : 'a option ;
    west  : 'a option list ;
  }

  type 'a surf = {
    ne    : xy ;
    sw    : xy ;
    stack : ('a arg) list ;
    north : 'a row list ;
    y     : int ; 
    herer : 'a row ;
    south : 'a row list ;
  }

  type 'a transducer = 'a surf -> 'a surf
  
  exception Empty_stack
    
  module Comb : sig 
    val empty : 'a surf
      
    (* No change to the surface. *)
    val id : 'a transducer
    val db : string -> 'a transducer

    (* pushing the argument stack. *)
    val push_arg : 'a arg -> 'a transducer
    val push_cur : 'a transducer
    val push_ne  : 'a transducer
    val push_sw  : 'a transducer
    
    (* Popping the argument stack. *)
    val op_1_arg : ('a arg -> 'a transducer) -> 'a transducer
    val op_2_arg : ('a arg -> 'a arg -> 'a transducer) -> 'a transducer
      
    (* Primtive movement transducers.  
       All are derived-forms of move_rel. *)
    val move_rel : 'a transducer
    val move_abs : 'a transducer
    val north    : 'a transducer
    val south    : 'a transducer
    val east     : 'a transducer
    val west     : 'a transducer

    (* val flatten  : ('a surf) surf -> 'a surf *)

    (* Manipulate data that's "here". *)
    val put  : 'a option -> 'a transducer
    val puts : 'a        -> 'a transducer
    val swap : ('a option -> 'a option) -> 'a transducer

    (* Create a list of lists, in row-major order. *)
    val dim     : 'a surf -> xy * xy
    val size    : 'a surf -> xy
    val get     : 'a surf -> 'a option
    val as_list : ne:xy -> sw:xy -> 'a surf -> (xy * 'a option) list list
  end 
    = 
  struct
    let empty = { 
      ne = (0,0) ;
      sw = (0,0) ;
      y = 0 ;
      stack = [] ;
      north = [];
      herer = { x    = 0; 
                here = None; 
                east = []; 
                west = []; } ;
      south = [] ; }

    let id z = z

    let db str z = 
      if debug_flag then (
        Printf.printf "%32s: sw=(%d,%d) cur=(%d,%d) ne=(%d,%d)\n" str
          (fst z.sw) (snd z.sw)
          (z.herer.x) z.y
          (fst z.ne) (snd z.ne) ; z
      ) else z
      
    let push_arg (xy,a) z = {z with stack=(xy,a)::z.stack}    
    let push_cur z = {z with stack=((z.herer.x, z.y), z.herer.here)::z.stack}      
    let push_ne  z = {z with stack=(z.ne, None)::z.stack}
    let push_sw  z = {z with stack=(z.sw, None)::z.stack}

    let op_1_arg f z = match z.stack with
      | []          -> raise Empty_stack
      | arg::stack' -> f arg {z with stack=stack'}

    let op_2_arg f z = match z.stack with
      | ( [] | _::[] )     -> raise Empty_stack
      | arg1::arg2::stack' -> f arg1 arg2 {z with stack=stack'}
          
    let sign a = 
      if      a == 0 then `Zero 
      else if a  > 0 then `Pos 
      else                `Neg 
        
    let min (a,b) = if a < b then a else b
    let max (a,b) = if a > b then a else b

    (* Move relative to a distance stored on the stack. *)
    let move_rel z = 
      op_1_arg begin fun ((dx,dy),_) z ->
        
        (* The absolute x coordinate that we need to go to. *)
        (* We need to save this here, because as we move vertically,
           the current-row's x coordinate will change. *)
        let abs_x = z.herer.x + dx in

        let rec loop_horz dx z =
          let step = function
            | cell :: cells -> cell, cells
            | []            -> None, []
          in
          match sign dx with
            | `Zero -> z           
            | `Pos  -> 
                (* Move east 1 unit *)
                let here', east' = step z.herer.east in
                loop_horz (dx - 1)
                  { z with 
                      ne = ( max(fst z.ne, z.herer.x + 1), snd z.ne ) ;
                      herer = { x    = z.herer.x + 1 ;
                                west = z.herer.here :: z.herer.west ;
                                here = here' ;
                                east = east' ; } }
                  
            | `Neg ->
                (* Move west 1 unit *)
                let here', west' = step z.herer.west in
                loop_horz (dx + 1)
                  { z with 
                      sw = ( min(fst z.sw, z.herer.x - 1), snd z.sw ) ;
                      herer = { x    = z.herer.x - 1 ;
                                west = west' ;
                                here = here' ;
                                east = z.herer.here :: z.herer.east ; } }
        in
        let rec loop_vert dy z =
          let step = function 
            | row::rows -> row, rows
            | []        -> {x=z.herer.x; east=[]; here=None; west=[]}, []
          in                
          match sign dy with
            | `Zero -> loop_horz (abs_x - z.herer.x) z
            | `Neg  ->
                (* Move south 1 unit *)
                let herer', south' = step z.south in
                loop_vert (dy + 1)
                  { z with 
                      y  = z.y - 1 ;
                      sw = ( fst z.sw, min(snd z.sw, z.y - 1) ) ;
                      herer = herer' ; 
                      south = south' ;
                      north = z.herer :: z.north }
                  
            | `Pos -> 
                (* Move north 1 unit *)
                let herer', north' = step z.north in
                loop_vert (dy - 1)
                  { z with 
                      y  = z.y + 1 ;
                      ne = ( fst z.ne, max (snd z.ne, z.y + 1) );
                      herer = herer' ; 
                      north = north' ;
                      south = z.herer :: z.south }
        in
        loop_vert dy z
      end z

      
    let move_abs z = 
      begin push_cur 
        **> op_2_arg begin fun ((x,y),_) ((ax,ay),_) ->
          begin push_arg ((ax - x, ay - y), None)
            **> move_rel
            **> db (Printf.sprintf "move_abs(%d,%d)" ax ay)
          end
        end
      end z

    let north  z = db "north" (move_rel (push_arg (( 0, 1), None) z))
    let south  z = db "south" (move_rel (push_arg (( 0,-1), None) z))
    let east   z = db "east"  (move_rel (push_arg (( 1, 0), None) z))
    let west   z = db "west"  (move_rel (push_arg ((-1, 0), None) z))
    let put  a z = {z with herer={z.herer with here=a}}

    let puts a z = 
      let z = db "puts" z in
      assert (z.herer.here = None ) ;
      {z with herer={z.herer with here=Some a}}

    let swap f z = put (f z.herer.here) z

    let dim surf = surf.ne, surf.sw
    
    let size surf = ( let ((nex,ney),(swx, swy)) = dim surf in
                      (nex - swx + 1, ney - swy + 1) )
      
    let get surf = surf.herer.here

    let as_list ~ne:ne ~sw:sw surf =
      let width  = fst ne - fst sw in
      let height = snd ne - snd sw in
      let surf = 
        begin push_arg (sw, None)
          **> push_arg (ne, None)
          **> op_2_arg begin 
            fun ((_,ney),_) ((swx,_),_) ->
              begin push_arg ((swx, ney),None) 
                **> move_abs
              end
          end
        end surf
      in
      let rec cells (x,y) surf = function
        | 0 -> []
        | n when n < 0 -> invalid_arg "cannot be negative"
        | n -> ((x,y), get surf) :: (cells (x+1,y) (east surf) (n-1))
      in
      let rec rows (x,y) surf = function
        | 0 -> []
        | n when n < 0 -> invalid_arg "cannot be negative"
        | n -> (cells (x,y) surf width) :: (rows (x,y+1) (south surf) (n-1))
      in
      ( rows (0,0) surf height )

  end
  include Comb
end
