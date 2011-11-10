(* Matthew Hammer <hammer@mpi-sws.org> *)

(* This module contains stuff that I'd like to put into Tv_signal.ml,
   if it weren't automatically generated from tv_signal.atd.  With
   this auto generation being the case, I instead put helper
   functions/modules/types here. *)

let trnode_id_of_decl : Tv_signal.decl -> Tv_signal.trnode_id =
  function
    | `D_allocated (t,_,_) -> t
    | `D_consistent t      -> t
    | `D_enqueued t        -> t
    | `D_dequeued t        -> t
    | `D_collected t       -> t
    | `D_freed t           -> t
        
        
(* - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

module Action : sig
  type t  = Tv_signal.action
  type id = Tv_signal.action_id

  val id  : t -> id option
  val id' : t -> id

  val trnode_of : t -> Tv_signal.trnode_id
  val has_nil_trnode : t -> bool
    
  val do_with_id_of : (id -> 'a) -> t -> 'a -> 'a

  val the_start : t
  val the_end : t
  val none : t


end = struct
  type t  = Tv_signal.action
  type id = Tv_signal.action_id

  let singleton id desc =
    ( { Tv_signal.ppt_id    = -1; 
        Tv_signal.ppt_file  = ""; 
        Tv_signal.ppt_fname = "";
        Tv_signal.ppt_line  = -1; 
        Tv_signal.ppt_byte  = -1 },
      id, -1, desc )

  let the_start = singleton ("start", "") (`A_push_begin)
  let the_end   = singleton ("end", "") (`A_push_end)
  let none      = singleton ("none", "") (`A_push_begin)

  let trnode_of (_, (trnode, _), _, _) = trnode

  let has_nil_trnode a =    
    match trnode_of a with
      | "0x0"   -> true
      | "(nil)" -> true
      | _       -> false

  let id (_, (trnode, handle), _, _) =
    match trnode,handle with        
  (* OLD Behavior: Some actions were missing unique IDs *)        
  (* NEW Behavior: All actions are assigned unique IDs by RT. *)
  (*    | "(nil)", _ -> None
      |  "0x0",  _ -> None
      |  "0",    _ -> None *)
      | _          -> Some (trnode, handle)

  let id' action = match id action with
    | Some aid -> aid
    | None     -> invalid_arg "id'"

  let do_with_id_of : (id -> 'a) -> t -> 'a -> 'a =
    fun f action default -> 
      match id action with 
        | None    -> default
        | Some id -> f id
end


module Action_keyelt = struct
  type t      = Action.id 
  let compare = Pervasives.compare 
end
  
module Action_setelt = struct
  type t          = Action.t
  let compare a b = compare (Action.id a) (Action.id b)
end


module Ap = struct (* Action Properties *)

  (* Mutually-exclusive states --- See: Tv_signal.decl *)
  type action_state =
    | As_allocated
    | As_consistent
    | As_enqueued 
    | As_dequeued
    | As_collected
    | As_freed
        
  (* Mutually-exclusive diff flags. *)
  type action_diff =
    | Ad_same     (* action is same before/after *)
    | Ad_diff     (* action is different, but present in both *)
    | Ad_only_old (* action is old, not present in new *)
    | Ad_only_new (* action is new, not present in old *)
        
  type properties = {
    state : action_state ;
    diff  : action_diff ;
  }

  type query = Action.t -> properties option

end


module Channel = struct
  
  type kind = 
    | Prop_phase
    | Core_phase
    | Meta_step
    | Runtime_step
    | Sasm_eval_step
    | Sasm_undo_step
    | Sasm_prop_step
    | Sasm_rewind_step
    | Sasm_rewind_full
    | Sasm_rewind_pop

  let string_of_kind = function
    | Prop_phase       -> "prop_phase"
    | Core_phase       -> "core_phase"
    | Meta_step        -> "meta_step"
    | Runtime_step     -> "runtime_step"
    | Sasm_eval_step   -> "sasm_eval_step"
    | Sasm_undo_step   -> "sasm_undo_step"
    | Sasm_prop_step   -> "sasm_prop_step"
    | Sasm_rewind_step -> "sasm_rewind_step"
    | Sasm_rewind_full -> "sasm_rewind_full"
    | Sasm_rewind_pop  -> "sasm_rewind_pop"

  let all_kinds = 
    [ Prop_phase ;
      Core_phase ;
      Meta_step ;
      Runtime_step ;
      Sasm_eval_step ;
      Sasm_undo_step ;
      Sasm_prop_step ;
      Sasm_rewind_step ;
      Sasm_rewind_full ;
      Sasm_rewind_pop ]

  type view = Pre | Post

  type tag = { kind : kind ;
               view : view }
      
  let tag kind view = {kind=kind ; view=view}

  module Tag_set = Set.Make ( 
    struct 
      type t      = tag 
      let compare = compare end
  )

  module Kind_set = Set.Make (
    struct
      type t = kind
      let compare = compare end
  )

  let universal_kind_set = 
    List.fold_right Kind_set.add all_kinds Kind_set.empty 

  let match_tags : Tag_set.t -> Kind_set.t -> bool =
    fun tags kinds ->
      let kinds_tags = Kind_set.fold 
        (fun kind kinds_tags -> 
           ( Tag_set.add (tag kind Pre)
               ( Tag_set.add (tag kind Post) kinds_tags
               ) ) )
        kinds Tag_set.empty 
      in
      not (Tag_set.is_empty (Tag_set.inter tags kinds_tags))
end

