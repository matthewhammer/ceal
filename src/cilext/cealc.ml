(***
Copyright 2008-2011 
Matthew A. Hammer <hammer@mpi-sws.org>

This file is part of the CEAL language implementation (CEAL for short).

CEAL is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

CEAL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with CEAL.  If not, see <http://www.gnu.org/licenses/>.
***)

module E = Errormsg
(** {3 Conventions } *)

(* NYI stands for Not-Yet-Implemented 
   
   The idea is to break off corner cases that are tricky to handle (or
   for which we haven't decided on solutions) and use an uniform
   exception to mark them as such (both textually--while developing
   the source, and dynamically--while running on example input
   programs).  Should only be used for "corner cases" that aren't
   exhibited by "non-corner case" programs.
*)
exception NYI
exception TODO

let ( **> ) f x = f x

(* TODO: remove this line and full-qualify the code appropriately *)
open Cil

(* pretty-printer that doesn't print line directives.  Useful for
   logging output where relevant location info is already prepended to
   each entry.  *)
class myCilPrinterClass = object(self)
  inherit defaultCilPrinterClass as super
    (*  method pLineDirective ?(forcefile=false) l = Pretty.nil *)
  (* method pStmt () (stmt:Cil.stmt) =
    Pretty.dprintf "<%d> %a" 
      stmt.Cil.sid (self#pStmt) stmt  *)
  method pAttr a =
    begin match a with
      | Cil.Attr(s,[]) 
          when Str.string_match (Str.regexp "ceal_\\(.+\\)") s 0 ->
          (* Give the ceal attributes more concise syntax *)
          (Pretty.dprintf "%s" (Str.matched_group 1 s)), false
      | _ -> (super#pAttr a)
    end
end

let my_printer = new myCilPrinterClass


(* helper-function for logging/debugging *)
let log (fmt: ('a,unit,Pretty.doc) format) : 'a =  
  let f d = ignore 
    (Pretty.eprintf "%t: %a"
       d_thisloc Pretty.insert d); Pretty.nil 
  in
  Pretty.gprintf f fmt


(** {3 Common} *)

let string_of_var var = var.Cil.vname

let string_of_label = function 
  | Cil.Label (s,_,_) -> s 
  | _ -> failwith "Non-Label labels shouldn't be around"
      
let string_of_exp exp = 
  Pretty.sprint ~width:80 (printExp my_printer () exp)
    
let string_of_lval lval =
  Pretty.sprint ~width:80 (printLval my_printer () lval)
    
let string_of_type typ =
  Pretty.sprint ~width:80 (printType my_printer () typ)

let string_of_global global =
  Pretty.sprint ~width:80 (printGlobal my_printer () global)

let string_of_attrs attrs =
  Pretty.sprint ~width:80 (printAttrs my_printer () attrs)

let string_of_attr attr =
  Pretty.sprint ~width:80 (printAttr my_printer () attr)

let string_of_attrparam attrparam =
  Pretty.sprint ~width:80 (my_printer#pAttrParam () attrparam)

let short_string_of_stmt stmt =
  let long = Pretty.sprint ~width:80 
    (Cil.printStmt my_printer () stmt) in
  if not (String.contains long '\n') then 
    long
  else 
    let nw_index = String.index long '\n' in
    String.sub long 0 nw_index


(* Program points -- our abstraction for (unique) program locations *)
module Program_point : sig
  type 'a t
  type fname = string
    
  (* Introduction forms and transformations. *)
  val none        : 'a t
  val root        : ?props:'a list -> Cil.location -> fname option -> 'a t
  val from        : 'a t -> 'a t
  val set_props   : ('a list -> 'b list) -> 'a t -> 'b t

  (* Comparison. *)
  val compare     : 'a t -> 'a t -> int

  (* Accessors. *)
  val loc         : 'a t -> Cil.location
  val id          : 'a t -> int
  val file        : 'a t -> string
  val line        : 'a t -> int
  val byte        : 'a t -> int
  val props       : 'a t -> 'a list
  val fname       : 'a t -> fname option

  (* Serialization. *)
  val doc         : unit -> 'a t -> Pretty.doc
  val edoc        : unit -> 'a t -> Pretty.doc (* "escaped", suitable for DOT-file IDs *)
  val long_doc    : unit -> 'a t -> Pretty.doc
  val string      : 'a t -> string
  val long_string : 'a t -> string
  val estring     : 'a t -> string
end
  = 
struct 
  type fname = string

  (* program points form trees -- Each program point has zero or one
     parent.  The trees induced by parent relationships represent the
     ancestry of program points being copied or transformed into other
     program points throughout the various compilation passes.  This
     ancestry will hopefully be useful to debug programs that arise
     from the interaction of a transformations when sequentially
     composed.  *)
  type 'a t = 
      { ppt_id     : int ;
        ppt_loc    : Cil.location ;
        ppt_fname  : fname option ;
        ppt_parent : 'a t option ;
        ppt_props  : 'a list ; 
      }
      
  let next = ref 0
    
  let root ?props:(props=[]) (loc:Cil.location) (fname:fname option)= 
    let id = !next in 
    incr next; 
    { ppt_id     = id ;
      ppt_loc    = loc ;
      ppt_fname  = fname ;
      ppt_parent = None ;
      ppt_props  = props ;
    }
      
  let none = root Cil.locUnknown None

  let from a = 
    let id = !next in
    incr next ;
    { ppt_id     = id ;
      ppt_loc    = a.ppt_loc ;
      ppt_fname  = a.ppt_fname ;
      ppt_parent = Some a ;
      ppt_props  = a.ppt_props }

  let set_props propf a = {
    ppt_id     = a.ppt_id          ;
    ppt_loc    = a.ppt_loc         ;
    ppt_fname  = a.ppt_fname       ;
    ppt_parent = None              ;
    ppt_props  = propf a.ppt_props ;
  }
      
  let strip a = set_props (fun _ -> ( [] : unit list )) a

  let compare a b = a.ppt_id - b.ppt_id

  let id    a = a.ppt_id
  let loc   a = a.ppt_loc
  let file  a = a.ppt_loc.Cil.file    
  let fname a = a.ppt_fname
  let byte  a = a.ppt_loc.Cil.byte
  let line  a = a.ppt_loc.Cil.line   
  let props a = a.ppt_props

  let edoc _ a = Pretty.dprintf "%s_%d" 
    (if a.ppt_loc.line > 0 then string_of_int a.ppt_loc.line else "x") a.ppt_id

  let doc _ a = Pretty.dprintf "<%d,#%d>" a.ppt_loc.line a.ppt_id
    
  let long_doc _ a = Pretty.dprintf "<%s:%d,#%d>" 
    a.ppt_loc.Cil.file a.ppt_loc.Cil.line a.ppt_id
    
  let estring a = Pretty.sprint ~width:80 (doc () a)
    
  let string a = Pretty.sprint ~width:80 (doc () a)

  let long_string a = Pretty.sprint ~width:80 (long_doc () a)
end

module Error = struct
  let fail (ppt : 'a Program_point.t) (msg:string) =
    failwith ( Printf.sprintf "%s: %s"
                 ( Program_point.long_string ppt) msg );    
end

module Abbrev = struct
  type loc   = Cil.location
  type label = Cil.label
  type var   = Cil.varinfo
  type vart  = Cil.varinfo * Cil.typ
  type vars  = Cil.varinfo list
  type field = Cil.fieldinfo
  type typ   = Cil.typ

  type pure  = Cil.exp  (* TODO: write a check for purity *)
  type lval  = Cil.lval (* TODO: write a sanity check: only Cil.Mem bases *)
  
  let lval_of_var   var = (Cil.Var var, Cil.NoOffset)
  let pure_of_lval lval = Cil.Lval lval
  let pure_of_var   var = pure_of_lval (lval_of_var var)
end


module Symgen = struct
  
  let amp = if false then "@" else ""

  let fresh_sym _ =
    let var = Cil.makeVarinfo false "" voidType in
    (Printf.sprintf "%stmp_%d" amp var.vid)

end

module Get_location = struct
  let of_stmt (s:Cil.stmt) = 
    Cil.get_stmtLoc s.Cil.skind
  
  let of_stmts (ss:Cil.stmt list) =
    match ss with 
      | s::_ -> of_stmt s
      | _    -> raise Not_found

  let of_block (b:Cil.block) =
    of_stmts b.Cil.bstmts

  let of_fundec (f:Cil.fundec) =
    of_block f.Cil.sbody
end

module Global_flags = struct
  
  let our_name : unit -> string = 
    fun _ -> "cealc"

  let output_path : string ref 
      = ref "./"

  let compilation_target : 
      [ `Self_adjusting 
      | `Verifier        ] ref = ref `Self_adjusting

  let string_of_compilation_target _ = 
    match !compilation_target with    
      | `Self_adjusting -> "self-adjusting"
      | `Verifier       -> "verifier"
          
  let announce_that_we_exist   = ref true
  let debug_passes             = ref false
  let debug_live_vars          = ref false
  let debug_entrances          = ref false
  let debug_cil_dom_trees      = ref false
  let debug_dom_trees          = ref false
  let destinations_are_awar    = ref false
  let singleton_trnodes        = ref false
  let dps_is_selective         = ref true
end

module Dot_util = struct

  let run name out_type out_ext =
    let command = Printf.sprintf 
      "dot %s.dot -T%s > %s.%s 2> %s.errors" 
      name out_type name out_ext name
    in
    match Unix.system command with
      | Unix.WEXITED (0|2) -> ()
    | _                  -> ignore 
        (E.warn "failed to run dot: `%s'\n" command)

  let dump 
      (path_wo_ext : string)
      (dump : out_channel -> 'a) : 'a 
      =    
    let dot_path = (path_wo_ext ^ ".dot") in
    let out      = open_out dot_path in
    let ret      = dump out in
    close_out out ;
    run path_wo_ext "ps"  "ps" ;
    run path_wo_ext "pdf" "pdf" ;
    ret
end


(* Cil is so awesome in so many ways.  The way it tries to support and
   encode initialization code is not one of them, at least not for our
   purposes.

   The behavior we want is the following: each time I ask for a global
   initializer, I get some handle on a *fresh* fundec that I can dump
   code into.  After being passed through our system, this fundec is
   no longer special---its just a meta-level function that happens to
   be called straight away in main (another meta-level function).
   After this code (and all other code) has been transformed and
   lowered, we may again want a (distinct) global initializer to dump
   some other code into.
   
   Here are some key points to our hack below:

   -- We don't want to become confused by non-unique names.  Each time
   we make a global init function we should be using a unique name.
   
   -- We don't want to become confused about which order different
   initializers occur in.  Somewhat arbitrarily, we choose to insert
   each subsequent initializer function *before* the older ones; this
   is convienent for the usage within this compiler.
   
   -- We don't want an initializer function that's missing a return
   statment-- this breaks CFG invariants that would otherwise hold.
*)
module Glob_init = struct
  
  let n = ref 0

  (* Note: this side-effects the file! *)
  let make (file:Cil.file) : Cil.fundec =
    
    (* Get the global initializer *)
    let fundec = Cil.getGlobInit file in        

    (* Revert the file into the "has no globinit" state by making the
       initializer function just like any other sort of function. *)
    begin
      if not file.globinitcalled then
        failwith "Couldn't insert initializer code; is main() missing?" ;
      
      (* Give it a return statement *)
      fundec.Cil.sbody <- 
        Cil.mkBlock 
        [Cil.mkStmt (Cil.Return (None, Cil.locUnknown))] ;

      (* Give it a fresh name *)
      fundec.Cil.svar.Cil.vname <- 
        Printf.sprintf "%s__%d" fundec.Cil.svar.Cil.vname (!n) ; 
      incr n ;
      
      (* Move it into the globals of the file. *)
      file.globals <- 
        file.globals @ [Cil.GFun (fundec, Cil.locUnknown)] ;
      
      (* Erase our memory of it being a "special" fundec. *)
      file.globinitcalled <- false ;
      file.globinit       <- None  ;
    end ;
    fundec
      
end

module Qual = struct  

  type t =      
      (* Foreign_c: Cannot change the representation: foriegn C code
         will use it.  Storage with this qualifier is accessed by Peek
         and Poke instructions in the IL. *)
    | Foreign_c        
        
    (* All the rest allow us to change the representation-- only CEAL
       code will access storage that carries these qualifiers.
       Storage with this qualifer is accesed by Read and Write in the
       IL.  Eventually, these instructions are lowered into
       invoke/revoke calls in the run-time library. *)
    | Awar
    | Zwzr
    | Owcr
    | Ring                

  let all = [ Foreign_c ; Awar; Zwzr; Owcr; Ring ]

  let qual_of_string : string -> t = function
    | "foreign_c" -> Foreign_c
    | "awar" -> Awar
    | "zwzr" -> Zwzr
    | "owcr" -> Owcr
    | "ring" -> Ring    
    | _      -> raise Not_found

  let string_of_qual : t -> string = function
    | Foreign_c -> "foreign_c"
    | Awar -> "awar"
    | Zwzr -> "zwzr"
    | Owcr -> "owcr"
    | Ring -> "ring"

  let string_of_qual_option : t option -> string = function
    | None   -> "?"
    | Some q -> string_of_qual q

  let qual_of_attr = function
    | Cil.Attr("ceal_foreign_c", []) -> Some Foreign_c
    | Cil.Attr("ceal_awar", []) -> Some Awar
    | Cil.Attr("ceal_zwzr", []) -> Some Zwzr
    | Cil.Attr("ceal_owcr", []) -> Some Owcr
    | Cil.Attr("ceal_ring", []) -> Some Ring
    | _                         -> None

  let attr_of_qual = function
    | Foreign_c -> Cil.Attr("ceal_foreign_c", [])
    | Awar -> Cil.Attr("ceal_awar", [])
    | Zwzr -> Cil.Attr("ceal_zwzr", [])
    | Owcr -> Cil.Attr("ceal_owcr", [])
    | Ring -> Cil.Attr("ceal_ring", [])

  let qual_of_type (t:Cil.typ) : t option =
    match (Cil.typeAttrs (Cil.unrollType t)) with
      | []      -> None
      | [attr]  -> qual_of_attr attr
      | _       -> None

  let type_without_qual (t:Cil.typ) : Cil.typ =
    (* Remove qualifiers from the type (but _not_ its subtypes) *)
    Cil.typeRemoveAttributes 
      (List.map (fun s -> "ceal_"^s) (List.map string_of_qual all))
      (Cil.unrollType t)
      
  let type_with_qual (t:Cil.typ) (q: t option) : Cil.typ =
    let add t = match q with 
      | Some q' -> (Cil.typeAddAttributes [attr_of_qual q']) t
      | None    -> t
    in
    add ( type_without_qual t )
      
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* Identifying Foreign C code  *)
  (* - - - - - - - - - - - - - - *)
  (* This is important to do correctly because foreign C code has an
     untraced semantics that we do not alter, while all other code is
     either traced (core program) or mutates with the traced storage
     (meta program).  Keeping these three semantics consistent (both
     alone, and with each other) crtically relies on carefully
     separating their code from one another. *)
      
  let lval_is_foreign_c (lval:Cil.lval) : bool =
    (qual_of_type (Cil.typeOfLval lval)) = (Some Foreign_c)

  (* "Foreign" functions can be written in CEAL, and separated out
     using Separate_foreign.  The difference between 'extern' and
     'foreign' is that, in the former, the code is not present in
     the compilation unit; in the latter, it is *)
  let var_is_foreign_c var =
    let Cil.Attr(name, _) = attr_of_qual Foreign_c in
    Cil.hasAttribute name var.Cil.vattr
      
  (* TODO -- make this less hacky *)
  let location_is_foreign_c : Cil.location -> bool = fun l ->
    let rec dir_is_foreign_c dirname = 
      if Filename.dirname dirname = dirname then 
        (* No more prefixes to look at.
           We didn't find a system directory *)
        false
      else 
        let system_dirs =
          [ "/usr/include" ; 
            "/usr/lib" ]
        in
        if List.exists (fun d -> d = dirname) system_dirs then true
        else dir_is_foreign_c (Filename.dirname dirname)
    in 
    ( dir_is_foreign_c (Filename.dirname l.Cil.file) )
        
  (* - - - - - - - - - - - - - - - - - - - - - - - *)
  (* For choosing the implicit qualifier of memory *)
  module Implicit : sig 
    val arg_spec : Arg.spec 
    val choose : t option -> t
  end 
    = 
  struct
    let qual : t ref = (ref Awar) (* Awar is the default. *)

    let arg_spec : Arg.spec =
      Arg.Symbol 
        ( List.map string_of_qual all,
          (fun s -> qual := qual_of_string s) )

    let choose = function
      | None   -> !qual
      | Some q -> q
  end

  (* - - - - - - - - - - - - - - - - - - - - - - *)
  (* For choosing the qualifiers of destinations *)
  module Dest : sig
    val arg_spec : Arg.spec
    val type_of_basetype : Cil.typ -> Cil.typ
  end
    = 
  struct
    let qual : t ref = (ref Owcr) (* Owcr is the default. *)
  
    let arg_spec : Arg.spec = 
      Arg.Symbol 
        ( List.map string_of_qual all,
          (fun s -> qual := qual_of_string s) )
        
    (* Coercion for the types of destination storage (see
       Region_analysis and Dest_passing modules). *)
    let type_of_basetype (t:Cil.typ) : Cil.typ = 
      ( type_with_qual t (Some (! qual ) ))
  end


  (* - - - - - - - - - - - - - - - - - - - *)
  (* Stripping qualifiers from the program *)
  module Strip = struct
        
    let ignored : t list ref = ref []
      
    let ignore_arg_spec : Arg.spec =
      Arg.Symbol
        ( List.map string_of_qual all,
          (fun s -> ignored := qual_of_string s :: (!ignored)) )

    (* Removes all the quals given.  Use ignored-list above to strip
       the quals being ignored from the command line. Use list all
       (above) to strip all qualifiers. *)
    class visitor quals = object(self)
      inherit Cil.nopCilVisitor
      method vattr (attr:Cil.attribute) = begin
        match qual_of_attr attr with 
          | None   -> Cil.ChangeTo [attr]
          | Some q -> 
              if List.mem q (quals)
              then Cil.ChangeTo []
              else Cil.ChangeTo [attr]                
      end
    end
      
    let process_file (quals:t list) (file:Cil.file) = 
      let visitor = new visitor quals in
      Cil.visitCilFile visitor file
        
  end (* Qual.Strip module. *)
end (* Qual module. *)


(* Recognizes and manipulates CEAL program annotations/keywords *)
module Annot = struct  

  (* Symbolic representation of our annotations/keywords *)
  (* NOTE: These datatypes are redefined in Zipcfg *)
  type kw = 
    | Propagate
    | Scope
    | Kill
    | Qual of Qual.t

    | Alloc of Cil.typ
    | Cut   of memo
    | Memo  of scope
    | Core  of Cil.varinfo

    | Redo | Undo

  and memo = 
    | Memo_yes of scope
    | Memo_no 
        
  and scope =
    | Scope_same
    | Scope_change of Cil.exp

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Shoe-horning} *)  
  (* I'm sure it breaks many Cil invariants *)

  (* UNUSED HERE -- See cabs2cil.ml *)
  let attrparam_of_exp : Cil.exp -> Cil.attrparam = 
    fun exp -> Cil.ASizeOfS (Cil.TSBase (Cil.TArray (Cil.TVoid [], Some exp, [])))

  let exp_op_of_attrparam : Cil.attrparam -> Cil.exp option = function
    | Cil.ASizeOfS (Cil.TSBase (Cil.TArray (_, Some exp, _))) -> Some exp
    | _                                                       -> None

  let scope_of_attrparam : Cil.attrparam -> scope = fun ap ->
    match exp_op_of_attrparam ap with 
      | Some e -> Scope_change e
      | None   -> failwith "expected a (shoehorned) scope expression"

  let memo_of_cut_attribute : Cil.attribute -> memo = function
    | Cil.Attr("ceal_cut",  [ ]) -> Memo_no
    | Cil.Attr("ceal_memo", [ ]) -> Memo_yes (Scope_same)
    | Cil.Attr("ceal_memo", [a]) -> Memo_yes (scope_of_attrparam a)
    | _ -> failwith "expected a (well-formed) cut/memo attribute."

  let scope_of_memo_attribute : Cil.attribute -> scope = function
    | Cil.Attr("ceal_memo", [ ]) -> Scope_same
    | Cil.Attr("ceal_memo", [a]) -> (scope_of_attrparam a)
    | _ -> failwith "expected a (well-formed) memo attribute."

  let block_kw_of_attr = function
    | Cil.Attr(("ceal_cut"|"ceal_memo"), _) as a -> Some (Cut (memo_of_cut_attribute a))
    | _                                              -> None      

  (* Map from attributes (type Cil.attribute) to keywords (type kw option) *)
  let kw_of_attr = function 
    | Cil.Attr("ceal_propagate", [])             -> Some Propagate
    | Cil.Attr("ceal_scope", [])                 -> Some Scope
    | Cil.Attr("ceal_kill", [])                  -> Some Kill
    | Cil.Attr("ceal_cut", _) as a               -> Some (Cut (memo_of_cut_attribute a))
    | Cil.Attr("ceal_memo", _) as a              -> Some (Memo (scope_of_memo_attribute a))
    | Cil.Attr("ceal_alloc", [Cil.ASizeOf t])    -> Some (Alloc t)
    | Cil.Attr("ceal_alloc", _               )   -> assert false 
        (* TODO: make this more graceful *)
        
    | attr when (Qual.qual_of_attr attr) <> None -> ( match Qual.qual_of_attr attr with
                                                        | Some q -> Some (Qual q)
                                                        | None   -> failwith "impossible!" )

    | _                                         -> None

  (* Check if a CIL expression is actually an encoding for one of our keywords. *)
  let kw_of_exp = function
    | Cil.SizeOf(Cil.TPtr(Cil.TVoid _, [attr])) -> 
        kw_of_attr attr (* could be 'cut, 'memo' or 'propagate' *)
    
    | Cil.CastE(Cil.TPtr(_, [attr]), z) when z = zero  -> 
        kw_of_attr attr (* could be 'alloc' or 'scope' *)
    
    | Cil.Lval
        (Cil.Mem 
           (Cil.CastE(Cil.TPtr (Cil.TFun _ as t, _), 
                      Cil.AddrOf (Cil.Var v, Cil.NoOffset))),
         Cil.NoOffset) -> 
        if Cil.hasAttribute "ceal_core" (Cil.typeAttrs t) 
        then Some (Core v) (* is 'core'-mode call *)
        else None (* is something else. *)

    | _  -> None

  let block_kw_of_exp = function
    | Cil.SizeOf(Cil.TPtr(Cil.TVoid _, [attr])) -> block_kw_of_attr attr (* could be cut/memo *)
    | _                                         -> None  

  let kw_is_core = function
    | Some(Core _) -> true
    | _            -> false

  let kw_is_cut = function
    | Some(Cut _) -> true
    | _           -> false
  
  let kw_is_memo = function 
    | Some(Memo _) -> true
    | _            -> false

  let kw_is_alloc = function
    | Some(Alloc(_)) -> true
    | _              -> false

  let kw_is_propagate = function
    | Some(Propagate) -> true
    | _               -> false

  let kw_is_kill = function
    | Some(Kill) -> true
    | _          -> false
        
  let kw_is_scope = function
    | Some(Scope) -> true
    | _           -> false

  let var_of_core = function
    | Some(Core v) -> v
    | _            -> invalid_arg "var_of_core"

  let type_of_alloc = function
    | Some(Alloc(t)) -> t
    | _              -> invalid_arg "type_of_alloc"

  let stmt_is_cut stmt = match stmt.skind with
    | Cil.If(exp,_,_,_) when (kw_is_cut(kw_of_exp exp)) -> true
    | _ -> false
        
  (** {3 Conversion between allocation types and the resulting pointer
      types returned by the allocation.} *)

  let ptr_type_of_alloc_type (typ:Cil.typ) = match typ with
    | Cil.TArray (typ, _, _) -> Cil.TPtr(typ, [])
    | typ                    -> Cil.TPtr(typ, [])

  let ptr_type_of_alloc_kw  kw  = ptr_type_of_alloc_type (type_of_alloc kw)
  let ptr_type_of_alloc_exp exp = ptr_type_of_alloc_kw   (kw_of_exp exp)

  let exp_of_kw = function
    | Alloc typ -> begin match ptr_type_of_alloc_type typ with
          | Cil.TPtr(t, _) -> Cil.CastE(Cil.TPtr(t, [Cil.Attr("ceal_alloc", [Cil.ASizeOf typ])]), Cil.zero)
          | _              -> assert false
      end
    | _ -> assert false

  (** {3 Type sequences encoded into functions' return types} *)

  (* Pack a list of types into a single (packed) Cil.typ *)
  let type_seq_pack (types:Cil.typ list) : Cil.typ = 
    let fresh_name _ =
      let var = Cil.makeVarinfo false "" voidType in
      (Printf.sprintf "%stmp_%d" Symgen.amp var.vid)
    in
    match types with
      | [t] -> t
      | [ ] -> Cil.voidType
      |  _  -> let ci : Cil.compinfo = 
           Cil.mkCompInfo true (fresh_name ())
             begin fun _ -> List.map 
               begin fun typ -> 
                 (fresh_name ()), typ, None, [], Cil.locUnknown
               end types 
             end [] 
         in Cil.TComp (ci, [])

  (* Unpack a list of types from a single (packed) Cil.typ *)
  let type_seq_unpack (typ:Cil.typ) : Cil.global option * Cil.typ list =
    match typ with        
      (* We pack sequences as composite types; each of these needs a global decl. *)
      | Cil.TComp (ci, []) ->           
          Some (Cil.GCompTag (ci, Cil.locUnknown)),
          List.map (fun f -> f.Cil.ftype) ci.cfields
          
      | Cil.TVoid _        -> None, []
      | t                  -> None, [t]

  (* True only if the type is identical to an arrow type *)
  let is_arrow_type : Cil.typ -> bool = 
    fun t -> match Cil.typeSig t with
      | Cil.TSFun _ -> true
      | _           -> false
      
  (* Unpack a Cil.TFun _ into a pair of type lists (arg types, return types) *)
  let arrow_type_unpack (arrow_type:Cil.typ) 
      : Cil.global option * Cil.typ list * Cil.typ list = 
    let arg_types = match arrow_type with
      | Cil.TFun(_, None,      _, _) -> []
      | Cil.TFun(_, Some args, _, _) -> List.map (fun (_, t, _) -> t) args
      | _ -> assert false
    in
    let global_op, ret_types = match arrow_type with
      | Cil.TFun(Cil.TVoid _, _, _, _) -> None, []
      | Cil.TFun(t,           _, _, _) -> type_seq_unpack t
      | _                              -> assert false
    in
    (global_op, arg_types, ret_types)


  (* Some (really disgusting) technical notes that guide how we
     translate from the Cil representation of our annotations into our
     custom Zipcfg representation:

     -- First, _before_ removing duplicate branches under the IF
     statements that encode Cuts, compute liveness and phi-functions.
     
     -- Next, after computing the liveness and phi-functions (see
     above), remove the duplicate branches for cuts; there isn't a
     deep reason for this, just a technical one: it makes it easier to
     parse the resulting dominator tree into abstract syntax for our
     intermediate representation. *)
  module Remove_duplicate_cut_branches = struct
    class visitor = object
      inherit nopCilVisitor
        
      method vstmt stmt = match stmt.skind with
        | Cil.If (exp,b1,b2,loc) when kw_is_cut (block_kw_of_exp exp) ->
            let empty_block = Cil.mkBlock([]) in
            (* We change the statement in place.  We don't use
               Cil.ChangeTo because the CIL mailing list recommends
               against it---Cil.ChangeTo on statements will, in general,
               invalidate the Goto references that may target the
               statements that are swapped out. ) *)
            stmt.Cil.skind <- Cil.If(exp, b1, empty_block, loc) ;
            Cil.DoChildren
              
        | _ -> Cil.DoChildren
    end
      
    let of_fundec (fundec:Cil.fundec) : unit =
      let visitor = new visitor in
      ignore (Cil.visitCilFunction visitor fundec)
  end    

  (** {3 Stripping all attributes out of the program} *)
  module Strip = struct
    class visitor = object(self)
      inherit Cil.nopCilVisitor
      method vattr (attr:Cil.attribute) =
        match kw_of_attr attr with 
          | None   -> Cil.ChangeTo [attr] 
          | Some _ -> Cil.ChangeTo []
    end
    let process_file (file:Cil.file) = 
      let visitor = new visitor in
      Cil.visitCilFile visitor file
  end
end


(* Creates temporary labels and variables in a number of convenient ways. *)
module Temps : sig
  val wildcard_var : varinfo
  val fresh_sym    : unit -> string
  val fresh_var    : typ -> varinfo
  val fresh_var'   : string -> typ -> varinfo
  val fresh_ver    : varinfo -> varinfo
  val fresh_ver'   : varinfo -> typ -> varinfo
  val func_var     : varinfo list -> typ list -> varinfo  
  val func_var'    : attributes -> varinfo list -> typ list -> varinfo

  val entry_label  : label
  val fresh_label  : location -> label
  val pick_label   : location -> label list -> (label * label list)
end 
  = 
struct
  let amp = Symgen.amp
  let fresh_sym = Symgen.fresh_sym

  let wildcard_var = Cil.makeVarinfo false "_" (Cil.voidType)
    
  let fresh_var typ =
    let var = Cil.makeVarinfo false "" (Qual.type_without_qual typ) in
    (var.vname <- Printf.sprintf "%stmp_%d" amp var.vid; var)

  let fresh_var' name typ =
    Cil.makeVarinfo false name (Qual.type_without_qual typ)

  let fresh_ver var0 =
    let var = Cil.makeVarinfo false "" (Qual.type_without_qual var0.vtype) in
    (var.vname <- Printf.sprintf "%s__%d" var0.vname var.vid; var)

  let fresh_ver' var0 typ =
    let var = Cil.makeVarinfo false "" (Qual.type_without_qual typ) in
    (var.vname <- Printf.sprintf "%s__%d" var0.vname var.vid; var)

  let func_var' attributes args return_types =
    let formals = List.map (fun v -> v.Cil.vname, v.Cil.vtype, v.Cil.vattr) args in
    let rettype = Annot.type_seq_pack return_types in
    let typ     = Cil.TFun (rettype, Some formals, false, []) in
    let var     = fresh_var typ in
    let _       = var.Cil.vattr <- attributes in
    ( var )

  let func_var args return_types = func_var' [] args return_types

  let next_label_id = ref 0
    
  let fresh_label loc = 
    let id = !next_label_id in 
    incr next_label_id ; 
    Label((Printf.sprintf "%sL_%d" amp id), loc, false)

  (* we use the same entry label for every graph's entry node *)
  let entry_label = fresh_label Cil.locUnknown
    
  (* Choose a representative label amongst a list of canidates, return
     the pick and the rest *)
  let rec pick_label loc = 
    let rec loop labels_out = function
      | []                             -> (fresh_label loc, List.rev labels_out)
      | ((Label _) as label) :: labels -> (label, (List.rev labels_out) @ labels)
      | label                :: labels -> loop (label :: labels_out) labels
    in loop []
end

(* Given an ordered type Ord, gives us sets of Ord, mappings over Ord,
   and some extra utility functions for these mappings and sets. *)
module OrdThing(Ord:sig type t val compare : t -> t -> int end) = struct
  module Ord = Ord
  module Map = Map.Make(Ord)
  module Set = Set.Make(Ord)

  let set_from_list (xs:Ord.t list) : Set.t = 
    List.fold_left 
      (fun set x -> Set.add x set)
      Set.empty xs

  let set_from_op_list (xs:Ord.t option list) : Set.t =
    List.fold_left
      (fun set -> function 
         | Some x -> Set.add x set 
         | None -> set)
      Set.empty xs
end

(* sets and mappings over CIL statements *)
module Stmt = OrdThing (
  struct type t = Cil.stmt 
         let compare s1 s2 = 
           Pervasives.compare s1 s2
  end)

(* sets and mappings over CIL variables *)
module Varinfo = OrdThing(
  struct type t = Cil.varinfo 
         let compare v1 v2 = 
           Pervasives.compare v1.vid v2.vid 
  end)

(* sets and mappings over CIL labels (but only the "unique" ones) *)
module Label = OrdThing(
  struct type t = Cil.label
         let compare l1 l2 =
           match (l1, l2) with
             | (Label _, Label _) -> Pervasives.compare l1 l2
             | _ -> raise (Invalid_argument ("Label is not 'real'"))
  end)

(* sets and mappings over CIL types *)
module Type = OrdThing(
  struct type t = Cil.typ
         let compare t1 t2 =
           Pervasives.compare t1 t2
  end)

(* SetMap:

   A common use of Sets and Maps in program analysis algorithms is to
   have a mapping function from A's to sets of B's which is built up
   iteratively.  A common operation is then the following: update the
   mapping M at M(A), adding B to target set M(A).  This is often
   written in psuedo-code as:
   
   M(A) <-- M(A) U {B}

   We call this operation [extend].  Without a functor like this, each
   pairing of a set and a map module requires writing a specialized
   [extend] function, as well as specialized versions of other common
   idioms: ([empty]) initialize the mapping for some domain, setting
   all target sets to the empty set and ([has]) check if the target
   set for some key contains some value.

   The functions [extend] and [has] cover the corner cases where the
   key argument is not present in the domain of the given mapping in
   the following uniform way: they treat these cases as if the key was
   present, and was mapped to the emptyset.
*)
module SetMap (S:Set.S) (M:Map.S) = struct
  module S = S
  module M = M  
  type t = S.t M.t (* "SetMap" is a mnuenomic for this type. *)
      
  (* extend the mapping by including a new value in the target set of
     the given key.  *)
  (* TODO(?): rename this 'add' ? *)
  let extend (key:M.key) (value:S.elt) (setmap:t) : t = 
    if M.mem key setmap 
    then M.add key (S.add value (M.find key setmap)) setmap
    else M.add key (S.singleton value) setmap

  (* Contract the mapping by excluding a value from the target set of
     the given key.  This is the dual operation to [extend]. *)
  let remove (key:M.key) (value:S.elt) (setmap:t) : t =
    if M.mem key setmap 
    then M.add key (S.remove value (M.find key setmap)) setmap
    else setmap      
      
  (* Check if value is in the target set of the given key *)
  let has (key:M.key) (value:S.elt) (setmap:t) : bool = 
    if M.mem key setmap 
    then S.mem value (M.find key setmap)
    else false      

  (* A map that sends the given keys to the empty set. *)
  let empty (domain:M.key list) : t =
    List.fold_left (fun setmap key -> (
                      M.add key S.empty setmap
                    )) M.empty domain
      
  (* Get the entire target set for a given key; 
     returns the empty set if key is not present. *)
  (* TODO(?): rename this 'find' ? *)
  let get (key:M.key) (setmap:t) : S.t =
    if M.mem key setmap 
    then M.find key setmap
    else S.empty

  (* merge two setmaps M1 and M2 into a third one M3 such that:
     B \in M3(A) <===>  B \in M1(A) OR B \in M2(A)
  *)
  let merge (setmap1:t) (setmap2:t) =
    M.fold begin fun key values setmap ->
      S.fold begin fun value setmap ->
        extend key value setmap 
      end values setmap 
    end setmap1 setmap2

  let equal (setmap1:t) (setmap2:t) =
    M.equal S.equal setmap1 setmap2

end

(* Compute free variables of CIL datatypes (Cil.exp and Cil.lval) *)
module Free_vars : sig
  val of_lval : Cil.lval -> Varinfo.Set.t
  val of_exp  : Cil.exp  -> Varinfo.Set.t    
end
  = 
struct
  module V = Varinfo.Set
  class visitor = object
    inherit nopCilVisitor        
    val mutable vars : V.t = V.empty
    method vvrbl var = vars <- V.add var vars; SkipChildren
    method get () = vars
  end
    
  let of_lval (lval:Cil.lval) = 
    let v = new visitor in 
    ignore (visitCilLval (v :> cilVisitor) lval); 
    v#get()
      
  let of_exp(exp:Cil.exp) = 
    let v = new visitor in 
    ignore (visitCilExpr (v :> cilVisitor) exp);
    v#get()
end

module Scalar = struct

  (* Build a (high-order) copy of an offset where a hole takes the
     place of Cil.NoOffset *)
  (* TODO: Where does this code belong? it seems generally useful. *)
  let rec offset_with_hole (offset:Cil.offset) : (Cil.offset -> Cil.offset) =
    let rec build (offhole: Cil.offset -> Cil.offset) = function
      | Cil.NoOffset        -> offhole
      | Cil.Field (fld,off) -> build (fun o -> offhole (Cil.Field(fld, o))) off
      | Cil.Index (idx,off) -> build (fun o -> offhole (Cil.Index(idx, o))) off
    in build (fun o -> o) offset

  (* Converts an initializer into a sequence of Set instructions *)
  let rec instrs_of_init 
      (loc:Cil.location)
      (hole:Cil.offset -> Cil.lval)       
      (init:Cil.init) 
      : Cil.instr list -> Cil.instr list 
    =
    match init with
      | Cil.SingleInit exp -> 
          fun instrs -> 
            Cil.Set ((hole NoOffset), exp, loc) :: instrs

      | Cil.CompoundInit (_, offsets_and_inits) ->
          List.fold_right begin fun (offset, init) ->
            let hole = (fun o -> hole (offset_with_hole offset o)) in
            instrs_of_init loc hole init 
          end offsets_and_inits

  (* TODO: where does this code belong? *)
  let length_info_of_array_type (szexp_op:Cil.exp option) : (Int64.t * Cil.ikind) =
    begin match szexp_op with 
      | None       -> assert false
      | Some szexp -> begin match Cil.constFold true szexp with
          | Cil.Const(Cil.CInt64(sz,ik,_)) -> (sz, ik)
          | _                              -> assert false
        end
    end

  (* TODO: where does this code belong? *)
  let length_of_array_type x = fst (length_info_of_array_type x)
  
  (* - - - begin code that belongs here - - - *)        
  
  (*
  let rec is_scalar_type (typ:Cil.typ) : bool = 
    match typ with
      | Cil.TInt   _ -> true
      | Cil.TFloat _ -> true
      | Cil.TPtr   _ -> true
      | Cil.TEnum  _ -> true
      | Cil.TBuiltin_va_list _ -> true (* TODO: we need to deep-copy this to the heap *)
      | Cil.TVoid  _ -> assert false (* illegal here: void type *)      
      | Cil.TFun   _ -> assert false (* illegal here: (non-pointer) function type *)
      | Cil.TNamed (t, _) -> is_scalar_type t.Cil.ttype (* resolve typedefs *)
      | Cil.TComp _  -> Cil.bitsSizeOf typ <= Cil.bitsSizeOf (Cil.voidPtrType)
      | Cil.TArray _ -> false
  *)

  (* TODO: eventually replace this function with a call to [decomp],
     which is more general and should be the canonical way to
     interpret how a [Cil.typ] maps into our dichotomy of scalars and
     non-scalars. *)    
  let is_scalar_type' (corner_case : Cil.typ -> bool) = 
    let rec r (typ:Cil.typ) : bool =
      match typ with
        | Cil.TInt   _ -> true
        | Cil.TFloat _ -> true
        | Cil.TPtr   _ -> true
        | Cil.TEnum  _ -> true
        | Cil.TBuiltin_va_list _ -> true (* TODO: we need to deep-copy this to the heap *)
        | Cil.TVoid  _ -> corner_case typ (* illegal here?: void type *)      
        | Cil.TFun   _ -> corner_case typ (* illegal here?: (non-pointer) function type *)
        | Cil.TNamed (t, _) -> r t.Cil.ttype (* resolve typedefs *)
        | Cil.TComp _  -> Cil.bitsSizeOf typ <= Cil.bitsSizeOf (Cil.voidPtrType)
        | Cil.TArray _ -> false
    in r
          
  let rec is_scalar_type   = is_scalar_type' (fun _ -> assert false)
  let rec is_scalar_type'' = is_scalar_type' (fun _ -> false)

  let assert_scalar_type (typ:Cil.typ) : typ =
    assert (is_scalar_type typ) ; typ

  (* Our representation for a scalar decomposition of a C type. *)
  type ('a, 'b) decomp =
    | Scalar of 'a * 'b
    | Struct of 'a * ('a, 'b) decomp list 
    | Union  of 'a * ('a, 'b) decomp * ('a, 'b) decomp list 
    | Array  of 'a * ('a, 'b) decomp * Int64.t * Cil.ikind

  (* compute a length for a type.  By default each scalar has unit
     size (one).  This convention can be changed via the optional
     argument [scalar_size]. *)
  let decomp_size 
      ?scalar_size:(scalar_size=fun _ _ -> Int64.one)
      (d:('a,'b) decomp) : Int64.t = 
    let rec walk sz = function
      | Scalar (a, b)         -> Int64.add sz (scalar_size a b)
      | Struct (_, ds)        -> List.fold_left walk sz ds
      | Union  (_, d, _)      -> walk sz d
      | Array  (_, d, len, _) -> (Int64.add sz (Int64.mul (walk Int64.zero d) len))
    in walk Int64.zero d
         
  (** {Scalar decomposition into offsets (sans any indices)} *)

  (* Given a [Cil.typ], produce a decomposition of type [decomp_w_offsets].
     
     We abstract out the array indicies from the offsets (each index
     is a [Cil.exp]).  A list of array indicies is needed to get a
     concrete scalar offset. This abstraction allows us to decompose
     memory operations over non-scalars with arrays into nested loops
     that use array indicies [each a Cil.exp] to parameterize the
     read/write offsets.  We don't have those loop variables in hand,
     but this representation allows us to fill them in later on.  *)
  exception Missing_array_size 
  exception Missing_array_index
  exception Toomany_array_index

  type decomp_w_offsets = (Cil.typ, Cil.exp list -> Cil.offset) decomp

  let decomp (typ:Cil.typ) : decomp_w_offsets =
    let rec decomp_rec (typ:Cil.typ) 
        (offset: Cil.exp list -> Cil.offset -> Cil.offset)
        = 
      match typ with        
        (* Scalar cases *)
        | Cil.TInt             _
        | Cil.TFloat           _
        | Cil.TPtr             _
        | Cil.TEnum            _
        | Cil.TBuiltin_va_list _ 
          -> Scalar (typ, (fun idxs -> offset idxs Cil.NoOffset))
            
        (* Some corner cases *)
        | Cil.TVoid            _ -> assert false
        | Cil.TFun             _ -> assert false
        | Cil.TNamed(t, _)       -> decomp_rec t.Cil.ttype offset
            
        (* Structures and unions *)
        | Cil.TComp ({Cil.cstruct=is_struct; Cil.cfields=fields}, _) -> 
            let field_decomps = List.map begin fun fieldinfo ->  
              decomp_rec fieldinfo.Cil.ftype 
                (fun idxs offset' -> offset idxs (Cil.Field (fieldinfo, offset')))
            end fields in
            if is_struct then Struct (typ, field_decomps)
            else begin match field_decomps with
              | h::tl -> Union (typ, h, tl) (* TODO: check that the variants are compatible *) 
              | []    -> assert false (* empty union *)
            end
              
        (* Arrays (with sizes given by a constant expression) *)
        | Cil.TArray (el_typ, None, _)        -> raise Missing_array_size
        | Cil.TArray (el_typ, Some sz_exp, _) ->
            let elt_decomp = decomp_rec el_typ begin
              fun idxs offset' -> match idxs with 
                | []        -> raise Missing_array_index
                | idx::idxs -> offset idxs (Cil.Index (idx, offset'))
            end in
            let (len, ikind) = length_info_of_array_type (Some sz_exp) in
            Array (typ, elt_decomp, len, ikind)
    in
    decomp_rec typ (fun idxs offset -> match idxs with 
                      | [] -> offset 
                      | _  -> raise Toomany_array_index)
end

(* "Foreign C" functions and type declarations can be written in CEAL,
   and separated out using Separate_foreign_c.  The difference between
   'extern' and 'foreign' is that, in the former, the code is not
   present in the compilation unit; in the latter, it is. *)
module Separate_foreign_c = struct

  (* Coerces all scalar types to foreign_c, even if these types are
     nested deeply inside other types. *)
  class visitor_1 = object
    inherit nopCilVisitor
    method vtype t = 
      try
        if Scalar.is_scalar_type'' t then
          Cil.ChangeDoChildrenPost 
            ((Qual.type_with_qual t (Some Qual.Foreign_c)),
             (fun x -> x))
        else
          Cil.DoChildren
      with          
          (* BUG-FIX: On Mac 10.6.8 -- Some incomplete-struct global
             variables exist.  Cannot take the size of these.  So, we
             just skip them and do not coerce anything (no fields to
             coerce anyway).  *)
        | Cil.SizeOfError _ -> Cil.SkipChildren
  end
    
  (* Coerce "foreign c" globals...by coercing their types. *)
  class visitor_2 = object
    inherit nopCilVisitor

    val mutable foreign : Cil.global list = []
      
    method get_foreign () = foreign

    method vglob g = 
      
      let do_type t =
        if Qual.location_is_foreign_c (Cil.get_globalLoc g) then 
          Cil.visitCilType (new visitor_1) t
        else t
      in
      
      match g with
        (* Seperate the foreign code out.  
           That way, we don't have to represent it in IL. *)
        | Cil.GFun (fundec, loc) when 
            (  Qual.var_is_foreign_c fundec.Cil.svar 
            || Qual.location_is_foreign_c (Cil.get_globalLoc g) )
            ->
            begin
              foreign <- g :: foreign ;
              Cil.ChangeTo [ Cil.GVarDecl ( fundec.Cil.svar, loc) ]
            end

        (* Typedefs *)
        | Cil.GType (ti, _) -> 
            ti.Cil.ttype <- do_type ti.Cil.ttype ;
            Cil.SkipChildren
              
        (* Structs (with fields) *)
        | Cil.GCompTag (ci, loc) ->
            List.iter (fun f -> f.Cil.ftype <- do_type f.Cil.ftype) 
              ci.Cil.cfields ;
            Cil.SkipChildren
              
        (* Variable declarations (with an initializer) *)
        (* Variable declarations (no initializer) *)
        | ( Cil.GVarDecl (vi, _) 
          | Cil.GVar (vi, _, _) ) ->
            (if not (Cil.isFunctionType vi.Cil.vtype) then
               vi.Cil.vtype <- do_type vi.Cil.vtype ) ;              
            Cil.SkipChildren
              
        (* Some global that we don't care about *)
        | _ -> Cil.SkipChildren
  end

  let process_file (file:Cil.file) =
    let v = new visitor_2 in
    Cil.visitCilFile (v :> cilVisitor) file ;
    ( v # get_foreign () )    

end

(* Some utilities used to heapify both global and local variables *)
module Heapify = struct
  
  module M = Varinfo.Map  
  type 'a subst = (Cil.varinfo option * 'a) M.t 
      
  (* TODO -- review the array-type corner cases *)
  let indirection 
      (orig_typ:Cil.typ) 
      (ptr_var:Cil.varinfo) 
      (orig_offset:Cil.offset) 
      : Cil.lval = 
    match orig_typ, orig_offset with
        (* corner cases arise when heapified variable has an array type *)
        | Cil.TArray _, Cil.NoOffset            -> (Cil.Var ptr_var, Cil.NoOffset)
        | Cil.TArray _, Cil.Field _             -> assert false
        | Cil.TArray _, Cil.Index (idx, offset) ->  
            let memexp = (* array indexing becomes pointer arithmetic *)
              (Cil.BinOp (Cil.IndexPI, 
                          Cil.Lval (Cil.Var ptr_var, Cil.NoOffset), 
                          idx, ptr_var.Cil.vtype))
            in
            (Cil.Mem memexp, offset)              
              
        (* the non-corner case *)
        | _ -> (Cil.Mem (Cil.Lval (Cil.Var ptr_var, Cil.NoOffset)), orig_offset)

  (* Visit an lval *)
  let vlval (subst:'a subst) lval = match lval with
    | (Cil.Mem _, _) -> DoChildren
    | (Cil.Var v, _) -> 
        begin try match fst (M.find v subst) with
          | None       -> Cil.DoChildren (* variable can stay *)
          | Some v_ptr -> let post = function 
              | (Cil.Var v', offset') when v == v' -> 
                  indirection v.Cil.vtype v_ptr offset'
              | _ -> assert false 
            in Cil.ChangeDoChildrenPost (lval, post)
        with Not_found -> Cil.DoChildren end

  (* Visit an expression *)            
  let vexpr (subst:'a subst) exp = match exp with
    | Cil.StartOf (Cil.Var v, Cil.NoOffset) ->
        begin try match fst (M.find v subst) with
          | Some v_ptr  -> Cil.ChangeTo (Cil.Lval (Cil.Var v_ptr, Cil.NoOffset))
          | _           -> Cil.DoChildren
        with Not_found  -> Cil.DoChildren end
          
    (* This bit isn't necessary, but it makes for simplier code in a
       common case where we are taking the address of a local variable
       that's been moved to the heap.  In these cases, we can elide
       the address-of (&) operation and just use our pointer temporary. *)
    | Cil.AddrOf (Cil.Var v, Cil.NoOffset) -> 
        begin try match M.find v subst with
          | (Some v_ptr, _) -> Cil.ChangeTo (Cil.Lval (Cil.Var v_ptr, Cil.NoOffset))
          | _               -> Cil.DoChildren
        with Not_found      -> Cil.DoChildren end
          
    | _                     -> Cil.DoChildren

end

(* Heapify_globals.
   
   Transforms the program so that all global variables are explicitly
   allocated in the heap.  

   As a post-condition, all global variables are (fixed) pointers that
   never change (though the memory they point to may, of course).
   This means that all mutation of global state uniformly uses Cil.Mem
   lvals (rather than Cil.Var lvals).  This property simplifies the
   cases we must handle later.

   BUG-FIX: Global variables that are declared as "extern" should not
   be moved.
*)

module Heapify_globals = struct

  module M = Varinfo.Map  
  module H = Heapify    

  class visitor globinitfundec = object(self)
    inherit Cil.nopCilVisitor

    val mutable subst = M.empty

    method vglob (glob:Cil.global) = 
      
      let temp_ptr (var:Cil.varinfo) =
        Cil.makeGlobalVar (Symgen.fresh_sym ())
          (Annot.ptr_type_of_alloc_type var.Cil.vtype)
      in

      match glob with
        | Cil.GVarDecl (v, loc) -> 
            if Cil.isFunctionType v.Cil.vtype 
              || v.Cil.vstorage == Cil.Extern 
              || Qual.var_is_foreign_c v
            then 
              Cil.DoChildren
            else
              if not (M.mem v subst) then 
                let v_ptr = temp_ptr v in
                subst <- M.add v (Some v_ptr, None) subst ;
                Cil.ChangeTo [Cil.GVarDecl(v_ptr, loc)]
              else
                Cil.ChangeTo []
                  
        | Cil.GVar (v, i, loc) ->
            let v_ptr = temp_ptr v in 
            begin if not (M.mem v subst) then 
              subst <- M.add v (Some v_ptr, Some i) subst
            else
              let (v_ptr, i0) = M.find v subst in
              begin match v_ptr, i0 with 
                | Some _, None   -> subst <- M.add v (v_ptr, Some i) subst
                | _,      Some _ -> assert false (* already has initializer? *)
                | None,   None   -> assert false (* this form shouldn't exist at all. *)
              end 
            end ;
            (* Replace the old GVar with an explicit allocation *)
            let alloc_exp = Annot.exp_of_kw (Annot.Alloc v.Cil.vtype) in
            let init = { Cil.init=Some(Cil.SingleInit alloc_exp) } in
            Cil.ChangeTo [Cil.GVar(v_ptr, init, loc)]

        | _ -> Cil.DoChildren
              
            
    method vlval (lval:Cil.lval) = H.vlval subst lval
    method vexpr (exp: Cil.exp)  = H.vexpr subst exp
      
    (* Generate the initialization instructions required for the
       globals we are moving *)
    method init_instrs loc instrs = 
      M.fold begin fun v (v_ptr_op, init_op) instrs ->
        
        match v_ptr_op, init_op with
            
          (* Redirected variable has an initializer *)
          | Some v_ptr, Some {Cil.init = Some init} -> 
              let hole o = (Cil.Mem (Cil.Lval (Cil.Var v_ptr, Cil.NoOffset)), o) in
              (fun is -> instrs (Scalar.instrs_of_init loc hole init is))

          (* Redirected variable (with no initializer). *)
          | Some v_ptr, (None | Some {Cil.init = None}) 
              -> instrs
              
          (* Corner cases don't arise. *)
          | None, Some _ -> assert false
          | None, None   -> assert false
              
      end subst (fun instrs -> instrs) instrs

    method vfunc (fundec:Cil.fundec) =
      if fundec == globinitfundec then 
        let _ = self#queueInstr 
          (self#init_instrs (Get_location.of_fundec fundec) []) in 
        Cil.DoChildren 
      else
        Cil.DoChildren
  end

  let process_file (file:Cil.file) = 
    let visitor = new visitor (Glob_init.make file) in
    let _       = Cil.visitCilFile (visitor :> cilVisitor) file in
    ()

end

(* Heapify_locals.

   Transforms the program to meet the following local variable
   criteria: each local variable of a function (including its
   parameters) is a scalar, and its address is never taken.  The local
   variable criteria is motivated by our SSA form, which assumes that
   locals are scalars that have no explicit storage (i.e., their
   address is never taken).

   For non-scalar locals as well as scalars whose address is taken, we
   make fresh allocations in the heap and use pointer indirection to
   access their contents.  We do this transformation on a function by
   function basis.  When processing a function, we:
   
   -- setup a fresh substitution for locals/parameters, mapping them
   to new temporary pointer variables, if they don't already meet the
   local variable criteria.
   
   -- replace lvalues with indirection through temporary pointer
   variables.
   
   -- insert some new preamble code which (1) initializes pointers
   with fresh allocations and (2) initializes these allocations to
   hold parameter values (in the case of a parameter being moved to
   the heap).

   TODO: We currently make the (simplifying) assumption that all
   functions already have scalar types (though their address may be
   taken).
*)
module Heapify_locals = struct

  (* relocated parameters require an additional copy instruction, 
     other locals don't *)
  type varkind = Param | Local
      
  exception Non_scalar_function_argument of Cil.fundec * string * Cil.varinfo * string

  module M = Varinfo.Map  
  type subst = (Cil.varinfo option * varkind) M.t 

  (* to heapify or not to heapify? *)
  let heapify_local (var:Cil.varinfo) : bool = 
    var.Cil.vaddrof || (not (Scalar.is_scalar_type var.Cil.vtype))

  class visitor = object(self)
    inherit Cil.nopCilVisitor
      
    val mutable fundec_subst : subst = M.empty
      
    method vfunc (fundec:Cil.fundec) = 
      
      let temp_ptr (var:Cil.varinfo) =
          Cil.makeTempVar fundec 
            (Annot.ptr_type_of_alloc_type var.Cil.vtype)
      in
      
      let subst = M.empty in

      (* Create substitution mapping local vars to either 
         new pointer vars or None *)
      let subst =
        List.fold_left begin fun subst local_var -> 
          if heapify_local local_var 
          then M.add local_var (Some (temp_ptr local_var), Local) subst
          else M.add local_var (None, Local) subst
        end subst fundec.Cil.slocals 
      in
      
      (* Extend substitution for parameter variables --- 
         we currently insist that these are all scalars *)
      let subst = 
        List.fold_left begin fun subst param_var ->
          if not ( Scalar.is_scalar_type param_var.Cil.vtype ) then
            raise (Non_scalar_function_argument(fundec, fundec.svar.vname, 
                                                param_var, param_var.vname))
          else
            if heapify_local param_var 
            then M.add param_var (Some (temp_ptr param_var), Param) subst
            else M.add param_var (None, Param) subst
        end subst fundec.Cil.sformals
      in

      (* Create instructions to allocate & initialize the heap copies
         for the heapified variables. *)
      let preamble_stmt loc = Cil.mkStmt (Cil.Instr (List.rev (
        M.fold begin fun orig_var (ptr_var, var_kind) instrs ->
          match ptr_var with 
            | None -> instrs
            | Some ptr_var ->
                (* This assertion does not hold for arrays being heapified: *)
                (* let typsig_1 = (Cil.typeSig (Cil.TPtr(orig_var.Cil.vtype, []))) in
                let typsig_2 = (Cil.typeSig ptr_var.Cil.vtype) in
                let _        = assert (typsig_1 = typsig_2) in *)
                let alloc_instr = 
                  Cil.Set ((Cil.Var ptr_var, NoOffset), 
                           Annot.exp_of_kw (Annot.Alloc orig_var.Cil.vtype), 
                           loc) 
                in                 
                begin match var_kind with
                  | Local -> alloc_instr :: instrs
                  | Param -> 
                      let init_instr =
                        Cil.Set ((Cil.Mem (Cil.Lval (Cil.Var ptr_var, Cil.NoOffset)), Cil.NoOffset),
                                 Cil.Lval(Cil.Var orig_var, Cil.NoOffset), 
                                 loc)
                      in
                      (* note: we build the instruction list in reverse order *)
                      init_instr :: alloc_instr :: instrs
                end (* end match *)
        end (* end fold fun *) subst [])))
      in
      fundec_subst <- subst ;            
      (* process the body using the substitution we just computed.
         Afterwards, insert extra allocation/initialization code. *)
      Cil.ChangeDoChildrenPost 
        (fundec, begin fun fundec ->
           {fundec with Cil.sbody = 
               {fundec.Cil.sbody with
                  Cil.bstmts = (preamble_stmt (Get_location.of_fundec fundec))
                   :: fundec.Cil.sbody.Cil.bstmts  
               } } end)
        
    (* Insert pointer indirection for each lvalue that mentions one of
       the locals/params we're moving to the heap. *)
    method vlval (lval:Cil.lval) = match lval with
      | (Cil.Mem _, _) -> DoChildren
      | (Cil.Var v, _) -> 
          begin try match fst (M.find v fundec_subst) with
            | None       -> Cil.DoChildren (* variable can stay *)
            | Some v_ptr -> let post = function 
                | (Cil.Var v', offset') when v == v' -> 
                    Heapify.indirection v.Cil.vtype v_ptr offset'
                | _ -> assert false 
              in Cil.ChangeDoChildrenPost (lval, post)
          with Not_found -> Cil.DoChildren end
        
    (* Update occurances of lvals in StartOf expressions (implicit
       coercion from Cil.TArray type to Cil.Ptr type is no longer
       necessary nor correct) *)
    method vexpr (exp:Cil.exp) = match exp with
      | Cil.StartOf (Cil.Var v, Cil.NoOffset) ->
          begin try match M.find v fundec_subst with
            | (Some v_ptr, _) -> Cil.ChangeTo (Cil.Lval (Cil.Var v_ptr, Cil.NoOffset))
            | _               -> Cil.DoChildren
          with Not_found      -> Cil.DoChildren end

      (* This bit isn't necessary, but it makes for simplier code in a
         common case where we are taking the address of a local variable
         that's been moved to the heap.  In these cases, we can elide
         the address-of (&) operation and just use our pointer temporary. *)
      | Cil.AddrOf (Cil.Var v, Cil.NoOffset) -> 
          begin try match M.find v fundec_subst with
            | (Some v_ptr, _) -> Cil.ChangeTo (Cil.Lval (Cil.Var v_ptr, Cil.NoOffset))
            | _               -> Cil.DoChildren
          with Not_found      -> Cil.DoChildren end
      
      | _                     -> Cil.DoChildren

  end (* visitor class *)

  let process_file (file:Cil.file) = 
    let visitor = new visitor in
    Cil.visitCilFile visitor file
end (* Heapify_locals module *)



module Scalarize_memops = struct
  
  (* array_threshold: a magic number that affects how we scalarize
     array-typed reads and writes. If the number of scalar ops to
     handle an array copy is less than (or equal to) this number, we
     expand the code to include each one (creating straight-line
     code); otherwise, we generate a new loop in the code *)
  let array_threshold = 16

  let stmt_of_instrs (instrs:Cil.instr list) : Cil.stmt =
    Cil.mkStmt (Cil.Instr instrs)    

  let stmt_of_instr (instr:Cil.instr) : Cil.stmt = 
    stmt_of_instrs [instr]
      
  let stmt_of_stmts (stmts:Cil.stmt list) : Cil.stmt = 
    Cil.mkStmt (Cil.Block (Cil.mkBlock stmts))      

  let decomp_nonscalar_set_instr 
      (fundec:Cil.fundec) (loc:Cil.location) 
      (left:Cil.lval) (right:Cil.exp) : (Cil.stmt) 
      =
    assert (not (Scalar.is_scalar_type (Cil.typeOfLval left))) ;
    assert (not (Scalar.is_scalar_type (Cil.typeOf right))) ;

    let with_hole = Scalar.offset_with_hole in
    
    (* left-hand pointer and offset *)
    let l_ptrexp, l_baseoff = 
      match left with 
        | (Cil.Var _, _) -> assert false (* assume pointer indirection *)
        | (Cil.Mem p, o) -> (p, with_hole o)
    in
    (* right-hand pointer and offset *)
    let r_ptrexp, r_baseoff =
      match right with
        | Cil.Lval(Cil.Var v, o) -> (Cil.Lval (Cil.Var v, NoOffset), with_hole o)
        | Cil.Lval(Cil.Mem p, o) -> (p,                              with_hole o)
        | _                      -> assert false (* no other non-scalar cases *)
    in
    (* walk over the decomposition *)
    let rec walk_decomp (idxs:Cil.exp list) 
        : Scalar.decomp_w_offsets -> Cil.stmt 
      = function
      (* scalars -- build a new [Cil.Set] instruction (at a scalar type). *)
      | Scalar.Scalar (typ, offset) -> 
          stmt_of_instr begin
            (* TODO -- seperate reads and writes into distinct instructions *)
            (* Perhaps seperation of memops should be postponed and done uniformly *)
            Cil.Set ((Cil.Mem l_ptrexp, l_baseoff (offset idxs)),
                     Cil.Lval (Cil.Mem r_ptrexp, r_baseoff (offset idxs)), 
                     loc) end
            
      (* structs -- walk over each struct component's decomposition *)
      | Scalar.Struct (_, decomps) -> 
          stmt_of_stmts (List.map (walk_decomp idxs) decomps)

      (* unions -- use the representative variant of the union *)
      (* TODO: we assume union variants have "compatible layouts" *)
      | Scalar.Union (_, decomp, _) -> walk_decomp idxs decomp
          
      (* arrays -- either generate straightline code or a loop *)
      | Scalar.Array (typ, decomp, len, ikind) as array_decomp -> 
          let exp_of_num num = Cil.kinteger64 ikind num in
          let exp_of_var var = Cil.Lval(Cil.Var var, Cil.NoOffset) in
          let sz = Scalar.decomp_size array_decomp in
          if Int64.compare sz (Int64.of_int array_threshold) <= 0
          then (* generate straightline code *)
            let rec loop (idx:Int64.t) = 
              match Int64.compare idx len with
                | -1 -> ((walk_decomp ((exp_of_num idx)::idxs) decomp) 
                         :: loop (Int64.succ idx))
                |  0 -> []
                |  _ -> assert false
            in
            stmt_of_stmts (loop Int64.zero)

          else (* create a loop *)
            let idx = Cil.makeTempVar fundec (Cil.TInt (ikind, [])) in
            stmt_of_stmts begin
              Formatcil.cStmts "idx = 0; while(idx < %e:len) { %s:body idx = idx + 1; }"
                (fun name typ -> Cil.makeTempVar ~name fundec typ) loc
                [("idx",  Cil.Fv idx); 
                 ("len",  Cil.Fe (exp_of_num len));
                 ("body", Cil.Fs (walk_decomp ((exp_of_var idx)::idxs) decomp))]
            end
    in
    walk_decomp [] (Scalar.decomp (Cil.typeOfLval left))

  (* From a [Cil.instr], generate a [Cil.stmt] with the same effect
     but where all memory operations occur at scalar types. *)
  let do_instr (fundec:Cil.fundec) (instr:Cil.instr) : Cil.stmt =
    match instr with 
        
      (* Assume that variables are scalars; no decomposition necessary *)
      | Cil.Set((Cil.Var v, Cil.NoOffset), exp, _) -> 
          assert (Scalar.is_scalar_type v.Cil.vtype) ;
          assert (Scalar.is_scalar_type (Cil.typeOf exp)) ;
          stmt_of_instr instr

      (* Assume that variables are scalars; hence, offsets are illegal *)
      | Cil.Set((Cil.Var _, _), _, _) -> 
          assert false (* non-scalars should already be heapified *) 

      (* Memory operations may need decomposition (if done on non-scalars). *)
      | Cil.Set(lval, exp, loc) -> 
          let lval_is_scalar = Scalar.is_scalar_type (Cil.typeOfLval lval) in
          let exp_is_scalar  = Scalar.is_scalar_type (Cil.typeOf exp) in          
          begin match lval_is_scalar, exp_is_scalar with
            | true,  true   -> stmt_of_instr instr
            | true,  _      -> assert false (* type mismatch *)
            | _,     true   -> assert false (* type mismatch *)
            | false, false  -> decomp_nonscalar_set_instr fundec loc lval exp
          end
            
      (* Function calls -- TODO: for now assume scalar args/result types *)
      | Call(lvalop, fexp, exps, loc) -> 
          List.iter (fun e -> assert (Scalar.is_scalar_type (Cil.typeOf e))) exps ;
          begin match lvalop with 
            | None      -> ()
            | Some lval -> assert (Scalar.is_scalar_type (Cil.typeOfLval lval)) 
          end ;
          stmt_of_instr instr

      (* Assembly is not supported *)
      | Asm _ -> raise NYI
          
  (* Expand sequences of (non-scalar) instructions with scalarized
     counterparts.  We do this at the statement level since we will
     use statements (e.g., loops) to express some expanded forms. *)
  class visitor = object(self)
    inherit Cil.nopCilVisitor
    val mutable fundec : Cil.fundec = Cil.dummyFunDec
    method vfunc (f:Cil.fundec) = fundec <- f; DoChildren
    method vstmt (stmt:Cil.stmt) = 
      match stmt.Cil.skind with
        | Cil.Instr instrs -> 
            (* TEMP-FIX: We are side-effecting the statement here. *)
            (* We can't do this functionally (nor with a Cil.ChangeTo)
               because Cil uses statement references to represent
               Cil.Goto statements; Hence, we can't replace statements
               with fresh ones without somewhow forwarding all those
               references to the fresh statement instance. *)
            assert (not (fundec == Cil.dummyFunDec)) ;
            let stmts = (List.map (do_instr fundec) instrs) in
            stmt.Cil.skind <- Cil.Block (Cil.mkBlock stmts) ;
            SkipChildren

        | _ -> DoChildren      
  end

  let process_file (file:Cil.file) = 
    let visitor = new visitor in
    Cil.visitCilFile visitor file
end

(* Transforms program so that each heap operation (each read, write or
   allocation) occurs in a distinct instruction.

   Assumes:
   -- variables adhere to the "local variable criteria" (see [Heapify_locals])
   -- Cil.Set instructions have been scalarized (see [Scalarize_memops])
*)
module Separate_memops = struct

  class visitor = object(self)
    inherit Cil.nopCilVisitor
    val mutable fundec : Cil.fundec   = Cil.dummyFunDec
    val mutable loc    : Cil.location = Cil.locUnknown
    
    method vglob _ = 
      fundec <- Cil.dummyFunDec ; 
      Cil.DoChildren

    method vfunc (f:Cil.fundec) = 
      fundec <- f; 
      Cil.DoChildren
    
    method vstmt (s:Cil.stmt) =
      loc <- Get_location.of_stmt s;
      Cil.DoChildren

    method vexpr (exp:Cil.exp) = begin match exp with

      (* Special case: Allocation Keyword -- 
         give pointer for allocation an explicit name *)
      | exp when fundec <> Cil.dummyFunDec 
          && Annot.kw_is_alloc (Annot.kw_of_exp exp) ->
          let tmp_typ = Annot.ptr_type_of_alloc_exp exp in
          let tmp_ptr = Cil.makeTempVar fundec tmp_typ in
          let instr   = Cil.Set((Cil.Var tmp_ptr, Cil.NoOffset), exp, loc) in
          self#queueInstr [instr] ;
          Cil.ChangeTo (Cil.Lval(Cil.Var tmp_ptr, Cil.NoOffset))
          
      (* simple recursive cases *)
      | ( Cil.Const     _  
        | Cil.SizeOf    _ 
        | Cil.SizeOfStr _ 
        | Cil.AlignOf   _ ) -> Cil.DoChildren

      | ( Cil.SizeOfE  _
        | Cil.AlignOfE _
        | Cil.AddrOf   _
        | Cil.StartOf  _ ) -> Cil.DoChildren 

      | ( Cil.UnOp     _ 
        | Cil.CastE    _ 
        | Cil.BinOp    _ ) -> Cil.DoChildren
          
      (* variable occurances with no offsets -- nothing to do. *)
      | Cil.Lval (Cil.Var _, Cil.NoOffset) -> Cil.SkipChildren
          
      (* illegal: variables are scalar-typed *)
      | Cil.Lval (Cil.Var _, _) -> assert false
          
      (* memory access -- create a seperate instruction *)
      | Cil.Lval (Cil.Mem _, _) -> 
          let post = fun exp -> 
            let exp_typsig = (Cil.typeSig (Cil.typeOf exp)) in
            match exp, exp_typsig with
              (* Corner case: use of function pointers.
                 These are special since they always point to immutable data (code). *)
              | (Cil.Lval (Cil.Mem _, Cil.NoOffset), Cil.TSFun _) -> exp
              | (Cil.Lval (Cil.Mem _, _),            Cil.TSFun _) -> assert false

              (* Reading a heap value *)
              | (Cil.Lval (Cil.Mem ptr_exp, offset), _) ->
                  let tmp_typ = Cil.typeOf exp in
                  let tmp_var = Cil.makeTempVar fundec tmp_typ in
                  let mem_ins = Cil.Set((Cil.Var tmp_var, Cil.NoOffset), 
                                        Cil.Lval(Cil.Mem ptr_exp, offset), loc) in
                  self#queueInstr [mem_ins] ;
                  Cil.Lval (Cil.Var tmp_var, Cil.NoOffset)
              | _ -> assert false

          (* first, fixup any nested expressions (the "child"
             expressions), then run [post] to separate this memory
             access to a seperate line. *)
          in Cil.ChangeDoChildrenPost (exp, post)
    end (* match *)
  end (* visitor class *)

  let process_file (file:Cil.file) = 
    let visitor = new visitor in
    Cil.visitCilFile visitor file
end

(* Prepares the cut blocks for CFG construction and dominator
   analysis.  Recall that they are embedded into the syntax of IF
   statements.  We "prepare" these statements by:

   -- Removing any code in the second branch (the ELSE branch) of the
   IF.  This is necessary since the THEN and ELSE branches are often
   duplicates of one another.

   -- TODO: Decompose branches (gotos) that target code outside of the
   cut into a series of two (or more) branches that encode these
   different control flow paths as data, (to leave the cut at one
   unique point) and then convert this data back to the intended
   control flow.
*)
module Prepare_cuts = struct

  let check_closed (block:Cil.block) : unit = 
    (* TODO: check that all the block is closed under control
       transfers (i.e., the block's entry and exit form a SESE region) *)
    ()

  class visitor = object
    inherit nopCilVisitor      
    method vstmt stmt = match stmt.skind with
      | Cil.If (exp,b1,b2,loc) when Annot.kw_is_cut (Annot.block_kw_of_exp exp) ->
          check_closed b1 ;
          let empty_block = Cil.mkBlock([]) in
          (* We change the statement in place.  We don't use
             Cil.ChangeTo because the CIL mailing list recommends
             against it---Cil.ChangeTo on statements will, in general,
             invalidate the Goto references that may target the
             statements that are swapped out. ) *)
          stmt.Cil.skind <- Cil.If(exp, b1, empty_block, loc) ;
          Cil.DoChildren

      | _ -> Cil.DoChildren
  end

  let process_fundec (fundec:Cil.fundec) : unit =
    let visitor = new visitor in
    ignore (Cil.visitCilFunction visitor fundec)

  let process_file (file:file) = 
    let visitor = new visitor in
    Cil.visitCilFileSameGlobals visitor file
end

module Cil_static_analysis = struct
  
  (* Gives the dominator tree as a actual data type that one can
     pattern-match on, as opposed to using an abstract type with a
     module signature for queuring the structure of the tree (as is done
     by the CIL [Dominators] module).
     
     Though the [Dominators] module provides a query operation for
     getting the children of a node ([Domninators.children]), it lacks
     any straight-forward means of getting the root of the tree.  Hence,
     writing algorithms that walk the tree are only "directly" supported
     by the iteration function [Dominators.domTreeIter], which is
     inherently imperative.  E.g., it lacks any means to fold a
     value--an accumulator for instance--through the iteration; the
     visitor function it receives returns unit.
     
     My response to this is to supplement the CIL [Dominators] module
     with the following one that gives the user direct access to the
     tree structure as a proper data type.
     
     Functions [compute] and [build] return the tree after either
     computing it from-scratch via [Dominators], or by using an existing
     [Dominators.tree] instance, respectively.  They return None only
     when the tree is empty.
  *)
  module Dom_tree : sig 
    type t = Tree of stmt * t list
    val build : Dominators.tree -> t option
    val compute : ?extra:string -> fundec -> t option 
  end 
    = 
  struct    
    type t = Tree of stmt * t list
      
    module D = Dominators

    exception Answer of stmt

    let build (tree0: Dominators.tree) =
      (* once we have the root, we recursively build using D.children *)
      let rec build stmt = Tree(stmt, List.map build (D.children tree0 stmt)) in
      (* find the root, if one exists; build the tree recursively starting there. *)
      match (try (D.domTreeIter (fun root -> raise (Answer root)) D.PreOrder tree0; None)
             with Answer(root) -> Some root)
      with 
        | None      -> None
        | Some root -> Some(build root)
            
    let dump out (tree:t option) =
      Printf.fprintf out "digraph {\n" ;
      let rec dump_rec (parent:Cil.stmt option) = function
        | Tree (s, trees) ->
            Printf.fprintf out "stmt_%d [label=\"%d:%s\"];\n" s.Cil.sid s.Cil.sid 
              (String.escaped (short_string_of_stmt s)) 
            ;
            begin match parent with
              | None        -> ()
              | Some parent -> Printf.fprintf out "stmt_%d -> stmt_%d;\n"
                  parent.Cil.sid s.Cil.sid 
            end 
            ;
            List.iter (dump_rec (Some s)) trees
      in 
      begin match tree with 
        | None   -> ()
        | Some t -> dump_rec None t
      end ;
      Printf.fprintf out "}"

    let compute ?extra:(extra="") (fundec:fundec) : t option =
      let (_, tree0)  = D.computeDomTree ~doCFG:false fundec in 
      let tree        = build tree0 in
      let _ = 
        if ! Global_flags.debug_cil_dom_trees then
          let path_wo_ext = Filename.concat 
            (!Global_flags.output_path) 
            (fundec.Cil.svar.Cil.vname ^ ".cil_dom_tree" ^ extra)
          in
          Dot_util.dump path_wo_ext (fun out -> dump out tree)
      in
      tree

  end


  (* Build the dominance frontier as a abstract data type that supports
     queries of the following form: "which statements are in the given
     statement's dominance frontier?".  
     
     We use a functional algorithm that is analogous to Figure 10 from
     Ron Cytron et. al's 1991 paper (entitled "Efficiently Computing
     Static Single Assignment Form and the Control Dependence Graph").
  *)
  module Dom_frontier : sig
    type t
    val compute : Dom_tree.t option -> t
    val get     : t -> stmt -> stmt list
    val stmts   : t -> stmt list
  end
    =
  struct
    module S = Stmt.Set
    module M = Stmt.Map    
      
    type t = S.t M.t
        
    let compute = 
      let rec walk (df:t) = function
        | Dom_tree.Tree(stmt, children) -> 
            let df = List.fold_left walk df children in                    
            let child_stmts = List.map (function Dom_tree.Tree(stmt,_) -> stmt) children in
            let stmt_df = 
              (S.diff
                 (S.union 
                    (Stmt.set_from_list stmt.succs)
                    (List.fold_left (fun stmts child -> S.union (M.find child df) stmts)
                       S.empty child_stmts))
                 (Stmt.set_from_list child_stmts))
            in
            (M.add stmt stmt_df df)
      in
      function 
        | None    -> M.empty
        | Some(t) -> walk M.empty t
            

    let get df stmt = S.elements (M.find stmt df)

    let stmts df = M.fold (fun stmt _ stmts -> stmt :: stmts) df []
  end

  (* A functional wrapper for Cil's (imperative) Liveness analysis *)
  (* NOTE: we do a second live-variable analysis after conversion into
     the Zipcfg representation (and after a few other transformations
     too); this second analysis is handled by a different module
     altogether, not the one below: *)
  module Live_sets : sig 
    type t
    val compute : fundec -> t
    val get     : t -> stmt -> varinfo list
  end 
    = 
  struct
    module A  = Annot
    module UD = Usedef
    module L  = Liveness
    module VS = Varinfo.Set
    module SM = Stmt.Map
    module LS = SetMap(VS)(SM)

    type t = LS.t

    let compute (fundec:fundec) = begin
      let saved_extraUsesOfExpr = ! UD.extraUsesOfExpr in
      
      (* We introduce some extra variable uses that Cil's use-def
         analysis doesn't ordinarly consider.  These appear within our
         shoehorned annotations (which themselves are tucked inside of
         attributes, etc).  *)
      UD.extraUsesOfExpr := begin fun exp ->
        (* Get the free-variables associated with a memo scope expression: *)
        let fv_of_scope_exp : A.scope -> UD.VS.t = function
          | A.Scope_same     -> UD.VS.empty
          | A.Scope_change e -> VS.fold UD.VS.add (Free_vars.of_exp e) UD.VS.empty
        in
        match Annot.kw_of_exp exp with
          | Some (A.Cut (A.Memo_yes s)) | Some (A.Memo s) -> fv_of_scope_exp s
          | _                                             -> UD.VS.empty
      end ;

      L.computeLiveness fundec ;
      UD.extraUsesOfExpr := saved_extraUsesOfExpr ;

      let foldf (ls:t) (stmt:stmt) = 
        let live_vars = Usedef.VS.elements (L.getLiveness stmt) in
        LS.M.add stmt (Varinfo.set_from_list live_vars) ls in
      List.fold_left foldf (LS.empty (fundec.sallstmts)) (fundec.sallstmts)
    end

    let get livesets stmt =
      VS.elements (SM.find stmt livesets)
  end
    
  (* Based on a given dominance frontier, compute the placement of phi
     functions/variables.  We compute a mapping from statements to the
     subset of local variables that require the use of a phi function at
     the head of the statement. 

     This is a functional version of Figure 11 of Cryton et. al. 1991.
     See full citation above.
  *)
  module Phi_funs : sig
    type t
    val compute : Dom_frontier.t -> t
    val get : t -> stmt -> varinfo list
  end
    =
  struct
    module DF = Dom_frontier
    module VS = Varinfo.Set
    module VM = Varinfo.Map
    module SS = Stmt.Set
    module SM = Stmt.Map

    module Assigns = SetMap(SS)(VM)
    module Phis = SetMap(VS)(SM)
      
    type t = Phis.t

    let compute (df:DF.t) : t =
      (* computes which variables are assigned-to in a given statement *)
      (* We assume that all locals meet the "local variable criteria" 
         -- namely, they are scalars whose address is not taken. *)
      let assign_vars_from_stmt (stmt:stmt) : VS.t =
        let assign_vars_from_instr = function
          | ( Set((Var v, NoOffset), _, _)
            | Call(Some(Var v, NoOffset), _, _, _) ) ->
              assert (not (Heapify_locals.heapify_local v)); Some v
          | _ -> None
        in
        match stmt.skind with
          | Instr instrs -> 
              (Varinfo.set_from_op_list 
                 (List.map assign_vars_from_instr instrs))
          | _ -> VS.empty
      in
      
      (* assigns: maps each varinfo to the set of statements that assign to it. *)
      let assigns : Assigns.t = 
        SS.fold (fun stmt assigns -> (
                   VS.fold (fun var assigns -> (
                              Assigns.extend var stmt assigns
                            ))
                     (assign_vars_from_stmt stmt)
                     assigns
                 ))        
          (Stmt.set_from_list (DF.stmts df))
          (Assigns.M.empty)
      in 

      let rec process_var (var:varinfo) (work_set:SS.t) (phis:t) : t =
        (* The work-set loop.  The [work_set] contains stmts with
           assignments to the given variable of interest, [var].  At a
           high-level, for each statement [stmt] in [work_set], we add
           phi functions for [var] to each statement [stmt_df] in the
           dominance frontier of [stmt].  For efficiency (and for
           termination) we maintain [done_set], containing the set of
           statements for which we've already added a phi function for
           [var].  We terminate when [work_set] is empty.  *)
        let rec work (work_set:SS.t) (done_set:SS.t) (phis:t) : t =
          if SS.is_empty work_set then phis
          else 
            let stmt     = SS.choose work_set in
            let work_set = SS.remove stmt work_set in
            let done_set = SS.add stmt done_set in

            (* For each statement [stmt_df] in the dominance frontier of
               [stmt], we consider adding a phi-function for [var] if we
               haven't done so already.  If we do this, we also add
               [stmt_df] to the [work_set], if it hasn't been done yet. *)
            let (phis, work_set) = 
              SS.fold (fun df_stmt (phis, work_set) -> (
                         if Phis.has df_stmt var phis 
                         then (phis, work_set)
                         else
                           (Phis.extend df_stmt var phis,
                            if SS.mem df_stmt done_set 
                            then work_set 
                            else SS.add df_stmt work_set)
                       ))
                (Stmt.set_from_list (DF.get df stmt))
                (phis, work_set)
            in
            (* loop until work is empty *)
            (work work_set done_set phis)
        in
        (* begin work loop: 
           work_set holds assigning stmts, 
           done_set is empty *)
        (work work_set SS.empty phis)
      in    
      (* For each variable that is assigned to, [process_var] it, being
         careful to thread through the "phi-function accumulator" (of
         type [Phis.t]), which becomes the result.  *)
      (Assigns.M.fold process_var assigns (Phis.empty (DF.stmts df)))
        
    let get (phis:t) (stmt:stmt) = 
      VS.elements (SM.find stmt phis)

  end
end (* end Cil_static_analysis module *)


(* Low : Symbolic representation of calls into the runtime library *)
module Low = struct
  open Abbrev
  
  type qtyp = Qual.t * typ

  type initflag = 
    | Initflag_none
    | Initflag_selfadj
    | Initflag_verifier
    | Initflag_tvsig

  let bit_of_initflag = function      
    (* WARNING -- 
       These numbers correspond to those in this header file:       
       src/lib/runtime/trace.h
       (It is critical that these two listings remain consistent.)
    *)
    | Initflag_none     -> 0
    | Initflag_selfadj  -> 1
    | Initflag_verifier -> 2
    | Initflag_tvsig    -> 4

  (* Create an int that encodes the given flags *)
  let bits_of_initflags flags = 
    List.fold_right 
      (fun flag bits -> bit_of_initflag flag lor bits) 
      flags 0

  (* Low ops *)            (* ARG1    ARG2    ARG3   ARG4   Return  *)
  type op =                (* ------  -----   -----  -----  ------  *)    
    | Init                 (*                               unit    *)

    | Core_begin           (*                               unit    *)
    | Core_end             (*                               unit    *)

    | Propagate            (*                               unit    *)

    | Trace_node           (* descr                         trnode  *)
    
    | Cut_begin            (*                               unit    *)
    | Cut_end              (*                               unit    *)
        
    | Update_invoke        (* -- phantom operation --               *)
        of var list        (* <-- annotated with live/key variables *)
    | Update_revoke        (* -- phantom operation --               *)

    | Memo_invoke          (* trnode, memoh,  bytes, bytec, ? ptr   *)
        of var list        (* <-- annotated with live/key variables *)

    | Memo_revoke          (* trnode, memoh                 unit    *)
    
    | Unboxed_invoke       (* trnode, offset, size          'a ptr  *) 
    | Unboxed_revinv       (* trnode, offset, size,         'a ptr  *)
    | Unboxed_revoke       (* trnode, offset, size,         unit    *)

    | Alloc_invoke         (* trnode, alloch, size          'a ptr  *)
    | Alloc_revinv         (* trnode, alloch, size          'a ptr  *)
    | Alloc_revoke         (* trnode, alloch,               unit    *)
    | Alloc_kill           (* ptr                           unit    *)
    
    | Scope_invoke         (* trnode, scopeh                void*   *)
    | Scope_revinv         (* trnode, scopeh                void*   *)
    | Scope_revoke         (* trnode, scopeh                unit    *)

    | Read_invoke of qtyp  (* trnode, readh,  'a ptr        'a      *)
    | Read_revinv of qtyp  (* trnode, readh,  'a ptr        'a      *)
    | Read_revoke of qtyp  (* trnode, readh,                unit    *)
    
    | Write_invoke of qtyp (* trnode, writeh, 'a ptr, 'a    unit    *)
    | Write_revinv of qtyp (* trnode, writeh, 'a ptr, 'a    unit    *)
    | Write_revoke of qtyp (* trnode, writeh,               unit    *)
        
    (* Generate a TV signal, using given a function and args *)
    | Tv_signal of string

(*
    | Abs_invoke of abs_op (* trnode, absh, ... *)
    | Abs_revinv of abs_op (* trnode, absh, ... *)
    | Abs_revoke of abs_op (* trnode, absh      *)
*)        
  module M = Map.Make(
    struct 
      type t = op 
      let compare op1 op2 = 
        match op1, op2 with
          | Memo_invoke _ , Memo_invoke _ ->  0
          | Memo_invoke _ , _             -> -1
          | _             , Memo_invoke _ ->  1
          | _             , _             ->  compare op1 op2
    end)

  let suffix_after prefix =
    let re = Str.regexp ((Str.quote prefix)^"\\(.*\\)") in
    fun s -> if Str.string_match re s 0 then 
      (Str.matched_group 1 s) else raise Not_found

  let qtype_of_op : op -> (Qual.t * typ) option = 
    function
      | Read_invoke qtyp  -> Some qtyp
      | Read_revinv qtyp  -> Some qtyp
      | Read_revoke qtyp  -> Some qtyp
      | Write_invoke qtyp -> Some qtyp
      | Write_revinv qtyp -> Some qtyp
      | Write_revoke qtyp -> Some qtyp
      | _                 -> None
      
  let name_of_op = 
    let st = string_of_type in
    let sq = Qual.string_of_qual in
    function
      | Init               -> "init"
      | Core_begin         -> "core_begin"
      | Core_end           -> "core_end"
      | Propagate          -> "propagate"
      | Trace_node         -> "trace_node"
      | Cut_begin          -> "cut_begin"
      | Cut_end            -> "cut_end"
      | Memo_invoke _      -> "memo_invoke"
      | Memo_revoke        -> "memo_revoke"
      | Update_invoke _    -> "update_invoke"
      | Update_revoke      -> "update_revoke"
      | Unboxed_invoke     -> "unboxed_invoke"
      | Unboxed_revinv     -> "unboxed_revinv"
      | Unboxed_revoke     -> "unboxed_revoke"
      | Alloc_invoke       -> "alloc_invoke"
      | Alloc_revinv       -> "alloc_revinv"
      | Alloc_revoke       -> "alloc_revoke"
      | Alloc_kill         -> "alloc_kill"
      | Scope_invoke       -> "scope_invoke"
      | Scope_revinv       -> "scope_revinv"
      | Scope_revoke       -> "scope_revoke"        
      | Read_invoke  (q,t) -> Printf.sprintf "read_invoke[%s,%s]"  (sq q) (st t)
      | Read_revinv  (q,t) -> Printf.sprintf "read_revinv[%s,%s]"  (sq q) (st t)
      | Read_revoke  (q,t) -> Printf.sprintf "read_revoke[%s,%s]"  (sq q) (st t)
      | Write_invoke (q,t) -> Printf.sprintf "write_invoke[%s,%s]" (sq q) (st t)
      | Write_revinv (q,t) -> Printf.sprintf "write_revinv[%s,%s]" (sq q) (st t)
      | Write_revoke (q,t) -> Printf.sprintf "write_revoke[%s,%s]" (sq q) (st t)
      | Tv_signal    s     -> Printf.sprintf "tvsig[%s]" s

  (* Try to get the closest corresponding read/write type for the
     given type.  This should always be a scalar type. *)
  let rec mem_op_type (ptr_type:Cil.typ) : Cil.typ =
    match Cil.typeSig ptr_type with
      | Cil.TSPtr _  -> Cil.voidPtrType
      | Cil.TSEnum _ -> mem_op_type (! Cil.upointType)
      | Cil.TSBase t -> t
      | _            -> invalid_arg "mem_op_type"
            
  (* Given the lval for a memory operation (either read or write),
     return the (closest-supported scalar) basetype for the operation,
     along with a pointer that has been cast appropriately *)
  let qual_basetype_pointer_of_lval (lval:Cil.lval) : Qual.t * typ * pure =
    let q_option = Qual.qual_of_type      (Cil.typeOfLval lval) in
    let q        = Qual.Implicit.choose   q_option in
    let t        = Qual.type_without_qual (mem_op_type (Cil.typeOfLval lval)) in
    if Cil.typeSig (Cil.typeOfLval lval) = Cil.typeSig t then 
      (q, t, Cil.AddrOf lval)
    else 
      (q, t, Cil.CastE (Cil.TPtr(t, []), Cil.AddrOf lval))

  (* low-level RT types.  This notion parallels the notion above
     of "low-level operations". *)
  type type_spec = 
    | T_desc 
    | T_redofn
    | T_undofn
    | T_desc_stats        
    | T_time 
    | T_scope
    | T_scopeh
    | T_memoh
    | T_updateh
    | T_alloch
    | T_modref of Qual.t * Cil.typ (* base-type for modref *)
    | T_readh  of Qual.t * Cil.typ (* base-type for read *)
    | T_writeh of Qual.t * Cil.typ (* base-type for write *)

  (* Print an error message that may be helpful. *)
  let qual_type_of_type_spec (ts : type_spec) = match ts with
    | T_modref (q,t) -> Some (q,t)
    | T_readh  (q,t) -> Some (q,t)
    | T_writeh (q,t) -> Some (q,t)
    | _              -> None

  (* Map low-level operations to their corresponding C runtime
     calls; Some read and write ops are parameterized by a scalar C
     type (the type of value being read or written). *)
  let vars : (var M.t) ref = ref M.empty

  exception Missing_runtime_var of string

  let var_of_op (ppt:'a Program_point.t) : op -> var = 
    fun op -> try M.find op (!vars) with
      | Not_found -> 
          let message = 
            Printf.sprintf "Error: Missing run-time support for: %s\n"
              (name_of_op op)
          in
          Error.fail ppt message

  (* Map handles to their corresponding C runtime types; Some handles
     are parameterized by a scalar C type (the type of value being
     read or written). *)
  module H = Hashtbl
  let typs : (type_spec, typ) H.t =
    H.create 13     

  exception Missing_runtime_type of type_spec
    
  (* Lookup the C type for the given type specification *)
  let type_of_type_spec (ppt:'a Program_point.t) : type_spec -> typ = fun ts ->
    try H.find typs ts with
      | Not_found -> 
          let message = 
            match qual_type_of_type_spec ts with
              | Some(q,t) -> Printf.sprintf
                  "Error: Missing run-time support for [%s,%s]\n"
                    (Qual.string_of_qual q) (string_of_type t)
              | None -> "Error: Missing needed run-time support here."
          in
          Error.fail ppt message
          
  (* Raise one of these exceptions if we "back-patch" any of the
     supported types more than once --- they witness an ambiguity in the
     runtime support (or the back-patching code below). *)
  exception Duplicate_runtime_type_spec of type_spec
  exception Duplicate_runtime_op of op

  let init file = begin
  (* Read through the given file and look for functions from the
     runtime header file.  Resolve the runtime variables and types
     that we will need to generate C code.  We place these C variables
     & types, into the vars and typs hashtables, respectively. *)
    let void_ptr_type = Cil.voidPtrType in
    let int_type      = Cil.intType in
    let uint_type     = Cil.uintType in
    let long_type     = Cil.longType in
    let ulong_type    = Cil.ulongType in
    let double_type   = Cil.doubleType in
    let float_type    = Cil.TFloat(Cil.FFloat, []) in

    let q_awar = Qual.Awar in
    let q_zwzr = Qual.Zwzr in
    let q_owcr = Qual.Owcr in
    let q_ring = Qual.Ring in

    begin Cil.iterGlobals file begin function
        (* Typedefs *)
      | Cil.GType (typeinfo, _) -> begin

          let add hspec =
            if H.mem typs hspec then
              raise (Duplicate_runtime_type_spec hspec)
            else
              H.add typs hspec (Cil.TNamed (typeinfo, []))
          in
          
          match typeinfo.Cil.tname with              
            (* (Non-parametric) types *)
            | "ceal_desc_t"       -> add T_desc
            | "ceal_redofn_t"     -> add T_redofn
            | "ceal_undofn_t"     -> add T_undofn
            | "ceal_desc_stats_t" -> add T_desc_stats
            | "ceal_time_t"       -> add T_time
            | "ceal_scope_t"      -> add T_scope
            | "ceal_scopeh_t"     -> add T_scopeh
            | "ceal_memoh_t"      -> add T_memoh
            | "ceal_updateh_t"    -> add T_updateh
            | "ceal_alloch_t"     -> add T_alloch
            
            (* Modrefs for AWAR Memory *)
            | "ceal_modref_awar_p_t"  -> add (T_modref (q_awar, void_ptr_type))
            | "ceal_modref_awar_i_t"  -> add (T_modref (q_awar, int_type))
            | "ceal_modref_awar_ui_t" -> add (T_modref (q_awar, uint_type))
            | "ceal_modref_awar_l_t"  -> add (T_modref (q_awar, long_type))
            | "ceal_modref_awar_ul_t" -> add (T_modref (q_awar, ulong_type))
            | "ceal_modref_awar_d_t"  -> add (T_modref (q_awar, double_type))
            | "ceal_modref_awar_f_t"  -> add (T_modref (q_awar, float_type))
                
            (* Read Handles for AWAR Memory *)
            | "ceal_readh_awar_p_t"  -> add (T_readh (q_awar, void_ptr_type))
            | "ceal_readh_awar_i_t"  -> add (T_readh (q_awar, int_type))
            | "ceal_readh_awar_ui_t" -> add (T_readh (q_awar, uint_type))
            | "ceal_readh_awar_l_t"  -> add (T_readh (q_awar, long_type))
            | "ceal_readh_awar_ul_t" -> add (T_readh (q_awar, ulong_type))
            | "ceal_readh_awar_d_t"  -> add (T_readh (q_awar, double_type))
            | "ceal_readh_awar_f_t"  -> add (T_readh (q_awar, float_type))

            (* Write Handles for AWAR Memory *)
            | "ceal_writeh_awar_p_t"  -> add (T_writeh (q_awar, void_ptr_type))
            | "ceal_writeh_awar_i_t"  -> add (T_writeh (q_awar, int_type))
            | "ceal_writeh_awar_ui_t" -> add (T_writeh (q_awar, uint_type))
            | "ceal_writeh_awar_l_t"  -> add (T_writeh (q_awar, long_type))
            | "ceal_writeh_awar_ul_t" -> add (T_writeh (q_awar, ulong_type))
            | "ceal_writeh_awar_d_t"  -> add (T_writeh (q_awar, double_type))
            | "ceal_writeh_awar_f_t"  -> add (T_writeh (q_awar, float_type))

            (* Modrefs for ZWZR Memory *)
            | "ceal_modref_zwzr_p_t"  -> add (T_modref (q_zwzr, void_ptr_type))
            | "ceal_modref_zwzr_i_t"  -> add (T_modref (q_zwzr, int_type))
            | "ceal_modref_zwzr_ui_t" -> add (T_modref (q_zwzr, uint_type))
            | "ceal_modref_zwzr_l_t"  -> add (T_modref (q_zwzr, long_type))
            | "ceal_modref_zwzr_ul_t" -> add (T_modref (q_zwzr, ulong_type))
            | "ceal_modref_zwzr_d_t"  -> add (T_modref (q_zwzr, double_type))
            | "ceal_modref_zwzr_f_t"  -> add (T_modref (q_zwzr, float_type))
                
            (* Read Handles for ZWZR Memory *)
            | "ceal_readh_zwzr_p_t"  -> add (T_readh (q_zwzr, void_ptr_type))
            | "ceal_readh_zwzr_i_t"  -> add (T_readh (q_zwzr, int_type))
            | "ceal_readh_zwzr_ui_t" -> add (T_readh (q_zwzr, uint_type))
            | "ceal_readh_zwzr_l_t"  -> add (T_readh (q_zwzr, long_type))
            | "ceal_readh_zwzr_ul_t" -> add (T_readh (q_zwzr, ulong_type))
            | "ceal_readh_zwzr_d_t"  -> add (T_readh (q_zwzr, double_type))
            | "ceal_readh_zwzr_f_t"  -> add (T_readh (q_zwzr, float_type))

            (* Write Handles for ZWZR Memory *)
            | "ceal_writeh_zwzr_p_t"  -> add (T_writeh (q_zwzr, void_ptr_type))
            | "ceal_writeh_zwzr_i_t"  -> add (T_writeh (q_zwzr, int_type))
            | "ceal_writeh_zwzr_ui_t" -> add (T_writeh (q_zwzr, uint_type))
            | "ceal_writeh_zwzr_l_t"  -> add (T_writeh (q_zwzr, long_type))
            | "ceal_writeh_zwzr_ul_t" -> add (T_writeh (q_zwzr, ulong_type))
            | "ceal_writeh_zwzr_d_t"  -> add (T_writeh (q_zwzr, double_type))
            | "ceal_writeh_zwzr_f_t"  -> add (T_writeh (q_zwzr, float_type))

            (* Modrefs for OWCR Memory *)
            | "ceal_modref_owcr_p_t"  -> add (T_modref (q_owcr, void_ptr_type))
            | "ceal_modref_owcr_i_t"  -> add (T_modref (q_owcr, int_type))
            | "ceal_modref_owcr_ui_t" -> add (T_modref (q_owcr, uint_type))
            | "ceal_modref_owcr_l_t"  -> add (T_modref (q_owcr, long_type))
            | "ceal_modref_owcr_ul_t" -> add (T_modref (q_owcr, ulong_type))
            | "ceal_modref_owcr_d_t"  -> add (T_modref (q_owcr, double_type))
            | "ceal_modref_owcr_f_t"  -> add (T_modref (q_owcr, float_type))

            (* Read Handles for OWCR Memory *)
            | "ceal_readh_owcr_p_t"  -> add (T_readh (q_owcr, void_ptr_type))
            | "ceal_readh_owcr_i_t"  -> add (T_readh (q_owcr, int_type))
            | "ceal_readh_owcr_ui_t" -> add (T_readh (q_owcr, uint_type))
            | "ceal_readh_owcr_l_t"  -> add (T_readh (q_owcr, long_type))
            | "ceal_readh_owcr_ul_t" -> add (T_readh (q_owcr, ulong_type))
            | "ceal_readh_owcr_d_t"  -> add (T_readh (q_owcr, double_type))
            | "ceal_readh_owcr_f_t"  -> add (T_readh (q_owcr, float_type))

            (* Write Handles for OWCR Memory *)
            | "ceal_writeh_owcr_p_t"  -> add (T_writeh (q_owcr, void_ptr_type))
            | "ceal_writeh_owcr_i_t"  -> add (T_writeh (q_owcr, int_type))
            | "ceal_writeh_owcr_ui_t" -> add (T_writeh (q_owcr, uint_type))
            | "ceal_writeh_owcr_l_t"  -> add (T_writeh (q_owcr, long_type))
            | "ceal_writeh_owcr_ul_t" -> add (T_writeh (q_owcr, ulong_type))
            | "ceal_writeh_owcr_d_t"  -> add (T_writeh (q_owcr, double_type))
            | "ceal_writeh_owcr_f_t"  -> add (T_writeh (q_owcr, float_type))

            (* Modrefs for RING Memory *)
            | "ceal_modref_ring_p_t"  -> add (T_modref (q_ring, void_ptr_type))
            | "ceal_modref_ring_i_t"  -> add (T_modref (q_ring, int_type))
            | "ceal_modref_ring_ui_t" -> add (T_modref (q_ring, uint_type))
            | "ceal_modref_ring_l_t"  -> add (T_modref (q_ring, long_type))
            | "ceal_modref_ring_ul_t" -> add (T_modref (q_ring, ulong_type))
            | "ceal_modref_ring_d_t"  -> add (T_modref (q_ring, double_type))
            | "ceal_modref_ring_f_t"  -> add (T_modref (q_ring, float_type))

            (* Read Handles for RING Memory *)
            | "ceal_readh_ring_p_t"  -> add (T_readh (q_ring, void_ptr_type))
            | "ceal_readh_ring_i_t"  -> add (T_readh (q_ring, int_type))
            | "ceal_readh_ring_ui_t" -> add (T_readh (q_ring, uint_type))
            | "ceal_readh_ring_l_t"  -> add (T_readh (q_ring, long_type))
            | "ceal_readh_ring_ul_t" -> add (T_readh (q_ring, ulong_type))
            | "ceal_readh_ring_d_t"  -> add (T_readh (q_ring, double_type))
            | "ceal_readh_ring_f_t"  -> add (T_readh (q_ring, float_type))

            (* Write Handles for RING Memory *)
            | "ceal_writeh_ring_p_t"  -> add (T_writeh (q_ring, void_ptr_type))
            | "ceal_writeh_ring_i_t"  -> add (T_writeh (q_ring, int_type))
            | "ceal_writeh_ring_ui_t" -> add (T_writeh (q_ring, uint_type))
            | "ceal_writeh_ring_l_t"  -> add (T_writeh (q_ring, long_type))
            | "ceal_writeh_ring_ul_t" -> add (T_writeh (q_ring, ulong_type))
            | "ceal_writeh_ring_d_t"  -> add (T_writeh (q_ring, double_type))
            | "ceal_writeh_ring_f_t"  -> add (T_writeh (q_ring, float_type))
                
            | _ -> ()
        end

      (* Function prototypes *)
      | Cil.GVarDecl (var, _) -> begin
        
        let add op =
          if M.mem op (!vars) then
            raise (Duplicate_runtime_op op)
          else
            (* Printf.printf "found: %s\n" var.vname ; *)
            vars := M.add op var (!vars) 
        in
        
        match var.Cil.vname with            
          (* Initialize *)
          | "ceal_init"         -> add Init

          (* Core program *)
          | "ceal_core_begin"   -> add Core_begin
          | "ceal_core_end"     -> add Core_end
              
          (* Propagate *)
          | "ceal_propagate"    -> add Propagate

          (* Trace nodes *)
          | "ceal_trnode_new"   -> add Trace_node
              
          (* Cuts *)
          | "ceal_cut_begin"    -> add Cut_begin
          | "ceal_cut_end"      -> add Cut_end

          (* Memoization *)
          | "ceal_memo_invoke"  -> add (Memo_invoke [])
          | "ceal_memo_revoke"  -> add Memo_revoke

          (* Update points *)
          | "ceal_update_invoke"  -> add (Update_invoke [])
          | "ceal_update_revoke"  -> add Update_revoke
                        
          (* Unboxed allocation *)
          | "ceal_unboxed_invoke" -> add Unboxed_invoke
          | "ceal_unboxed_revinv" -> add Unboxed_revinv
          | "ceal_unboxed_revoke" -> add Unboxed_revoke

          (* (boxed) Allocation *)
          | "ceal_alloc_invoke"  -> add Alloc_invoke
          | "ceal_alloc_revinv"  -> add Alloc_revinv
          | "ceal_alloc_revoke"  -> add Alloc_revoke
          | "ceal_alloc_kill"    -> add Alloc_kill

          (* Scope allocation *)
          | "ceal_scope_invoke"  -> add Scope_invoke
          | "ceal_scope_revinv"  -> add Scope_revinv
          | "ceal_scope_revoke"  -> add Scope_revoke
              
          (* Reads for AWAR Memory *)
          (*                       *)
          (* Read -- Pointers *)
          | "ceal_read_invoke_awar_p" -> add (Read_invoke (q_awar, void_ptr_type))
          | "ceal_read_revinv_awar_p" -> add (Read_revinv (q_awar, void_ptr_type))
          | "ceal_read_revoke_awar_p" -> add (Read_revoke (q_awar, void_ptr_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_awar_i" -> add (Read_invoke (q_awar, int_type))
          | "ceal_read_revinv_awar_i" -> add (Read_revinv (q_awar, int_type))
          | "ceal_read_revoke_awar_i" -> add (Read_revoke (q_awar, int_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_awar_ui" -> add (Read_invoke (q_awar, uint_type))
          | "ceal_read_revinv_awar_ui" -> add (Read_revinv (q_awar, uint_type))
          | "ceal_read_revoke_awar_ui" -> add (Read_revoke (q_awar, uint_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_awar_l" -> add (Read_invoke (q_awar, long_type))
          | "ceal_read_revinv_awar_l" -> add (Read_revinv (q_awar, long_type))
          | "ceal_read_revoke_awar_l" -> add (Read_revoke (q_awar, long_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_awar_ul" -> add (Read_invoke (q_awar, ulong_type))
          | "ceal_read_revinv_awar_ul" -> add (Read_revinv (q_awar, ulong_type))
          | "ceal_read_revoke_awar_ul" -> add (Read_revoke (q_awar, ulong_type))
          (* Read -- Floats *)
          | "ceal_read_invoke_awar_f" -> add (Read_invoke (q_awar, float_type))
          | "ceal_read_revinv_awar_f" -> add (Read_revinv (q_awar, float_type))
          | "ceal_read_revoke_awar_f" -> add (Read_revoke (q_awar, float_type))
          (* Read -- Doubles *)
          | "ceal_read_invoke_awar_d" -> add (Read_invoke (q_awar, double_type))
          | "ceal_read_revinv_awar_d" -> add (Read_revinv (q_awar, double_type))
          | "ceal_read_revoke_awar_d" -> add (Read_revoke (q_awar, double_type))

          (* Writes for AWAR Memory *)
          (*                        *)
          (* Write -- Pointers *)
          | "ceal_write_invoke_awar_p" -> add (Write_invoke (q_awar, void_ptr_type))
          | "ceal_write_revinv_awar_p" -> add (Write_revinv (q_awar, void_ptr_type))
          | "ceal_write_revoke_awar_p" -> add (Write_revoke (q_awar, void_ptr_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_awar_i" -> add (Write_invoke (q_awar, int_type))
          | "ceal_write_revinv_awar_i" -> add (Write_revinv (q_awar, int_type))
          | "ceal_write_revoke_awar_i" -> add (Write_revoke (q_awar, int_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_awar_ui" -> add (Write_invoke (q_awar, uint_type))
          | "ceal_write_revinv_awar_ui" -> add (Write_revinv (q_awar, uint_type))
          | "ceal_write_revoke_awar_ui" -> add (Write_revoke (q_awar, uint_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_awar_l" -> add (Write_invoke (q_awar, long_type))
          | "ceal_write_revinv_awar_l" -> add (Write_revinv (q_awar, long_type))
          | "ceal_write_revoke_awar_l" -> add (Write_revoke (q_awar, long_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_awar_ul" -> add (Write_invoke (q_awar, ulong_type))
          | "ceal_write_revinv_awar_ul" -> add (Write_revinv (q_awar, ulong_type))
          | "ceal_write_revoke_awar_ul" -> add (Write_revoke (q_awar, ulong_type))
          (* Write -- Floats *)
          | "ceal_write_invoke_awar_f" -> add (Write_invoke (q_awar, float_type))
          | "ceal_write_revinv_awar_f" -> add (Write_revinv (q_awar, float_type))
          | "ceal_write_revoke_awar_f" -> add (Write_revoke (q_awar, float_type))
          (* Write -- Doubles *)
          | "ceal_write_invoke_awar_d" -> add (Write_invoke (q_awar, double_type))
          | "ceal_write_revinv_awar_d" -> add (Write_revinv (q_awar, double_type))
          | "ceal_write_revoke_awar_d" -> add (Write_revoke (q_awar, double_type))

          (* Reads for ZWZR Memory *)
          (*                       *)
          (* Read -- Pointers *)
          | "ceal_read_invoke_zwzr_p" -> add (Read_invoke (q_zwzr, void_ptr_type))
          | "ceal_read_revinv_zwzr_p" -> add (Read_revinv (q_zwzr, void_ptr_type))
          | "ceal_read_revoke_zwzr_p" -> add (Read_revoke (q_zwzr, void_ptr_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_zwzr_i" -> add (Read_invoke (q_zwzr, int_type))
          | "ceal_read_revinv_zwzr_i" -> add (Read_revinv (q_zwzr, int_type))
          | "ceal_read_revoke_zwzr_i" -> add (Read_revoke (q_zwzr, int_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_zwzr_ui" -> add (Read_invoke (q_zwzr, uint_type))
          | "ceal_read_revinv_zwzr_ui" -> add (Read_revinv (q_zwzr, uint_type))
          | "ceal_read_revoke_zwzr_ui" -> add (Read_revoke (q_zwzr, uint_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_zwzr_l" -> add (Read_invoke (q_zwzr, long_type))
          | "ceal_read_revinv_zwzr_l" -> add (Read_revinv (q_zwzr, long_type))
          | "ceal_read_revoke_zwzr_l" -> add (Read_revoke (q_zwzr, long_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_zwzr_ul" -> add (Read_invoke (q_zwzr, ulong_type))
          | "ceal_read_revinv_zwzr_ul" -> add (Read_revinv (q_zwzr, ulong_type))
          | "ceal_read_revoke_zwzr_ul" -> add (Read_revoke (q_zwzr, ulong_type))
          (* Read -- Floats *)
          | "ceal_read_invoke_zwzr_f" -> add (Read_invoke (q_zwzr, float_type))
          | "ceal_read_revinv_zwzr_f" -> add (Read_revinv (q_zwzr, float_type))
          | "ceal_read_revoke_zwzr_f" -> add (Read_revoke (q_zwzr, float_type))
          (* Read -- Doubles *)
          | "ceal_read_invoke_zwzr_d" -> add (Read_invoke (q_zwzr, double_type))
          | "ceal_read_revinv_zwzr_d" -> add (Read_revinv (q_zwzr, double_type))
          | "ceal_read_revoke_zwzr_d" -> add (Read_revoke (q_zwzr, double_type))

          (* Writes for ZWZR Memory *)
          (*                        *)
          (* Write -- Pointers *)
          | "ceal_write_invoke_zwzr_p" -> add (Write_invoke (q_zwzr, void_ptr_type))
          | "ceal_write_revinv_zwzr_p" -> add (Write_revinv (q_zwzr, void_ptr_type))
          | "ceal_write_revoke_zwzr_p" -> add (Write_revoke (q_zwzr, void_ptr_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_zwzr_i" -> add (Write_invoke (q_zwzr, int_type))
          | "ceal_write_revinv_zwzr_i" -> add (Write_revinv (q_zwzr, int_type))
          | "ceal_write_revoke_zwzr_i" -> add (Write_revoke (q_zwzr, int_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_zwzr_ui" -> add (Write_invoke (q_zwzr, uint_type))
          | "ceal_write_revinv_zwzr_ui" -> add (Write_revinv (q_zwzr, uint_type))
          | "ceal_write_revoke_zwzr_ui" -> add (Write_revoke (q_zwzr, uint_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_zwzr_l" -> add (Write_invoke (q_zwzr, long_type))
          | "ceal_write_revinv_zwzr_l" -> add (Write_revinv (q_zwzr, long_type))
          | "ceal_write_revoke_zwzr_l" -> add (Write_revoke (q_zwzr, long_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_zwzr_ul" -> add (Write_invoke (q_zwzr, ulong_type))
          | "ceal_write_revinv_zwzr_ul" -> add (Write_revinv (q_zwzr, ulong_type))
          | "ceal_write_revoke_zwzr_ul" -> add (Write_revoke (q_zwzr, ulong_type))
          (* Write -- Floats *)
          | "ceal_write_invoke_zwzr_f" -> add (Write_invoke (q_zwzr, float_type))
          | "ceal_write_revinv_zwzr_f" -> add (Write_revinv (q_zwzr, float_type))
          | "ceal_write_revoke_zwzr_f" -> add (Write_revoke (q_zwzr, float_type))
          (* Write -- Doubles *)
          | "ceal_write_invoke_zwzr_d" -> add (Write_invoke (q_zwzr, double_type))
          | "ceal_write_revinv_zwzr_d" -> add (Write_revinv (q_zwzr, double_type))
          | "ceal_write_revoke_zwzr_d" -> add (Write_revoke (q_zwzr, double_type))
              
          (* Reads for OWCR Memory *)
          (*                       *)
          (* Read -- Pointers *)
          | "ceal_read_invoke_owcr_p" -> add (Read_invoke (q_owcr, void_ptr_type))
          | "ceal_read_revinv_owcr_p" -> add (Read_revinv (q_owcr, void_ptr_type))
          | "ceal_read_revoke_owcr_p" -> add (Read_revoke (q_owcr, void_ptr_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_owcr_i" -> add (Read_invoke (q_owcr, int_type))
          | "ceal_read_revinv_owcr_i" -> add (Read_revinv (q_owcr, int_type))
          | "ceal_read_revoke_owcr_i" -> add (Read_revoke (q_owcr, int_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_owcr_ui" -> add (Read_invoke (q_owcr, uint_type))
          | "ceal_read_revinv_owcr_ui" -> add (Read_revinv (q_owcr, uint_type))
          | "ceal_read_revoke_owcr_ui" -> add (Read_revoke (q_owcr, uint_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_owcr_l" -> add (Read_invoke (q_owcr, long_type))
          | "ceal_read_revinv_owcr_l" -> add (Read_revinv (q_owcr, long_type))
          | "ceal_read_revoke_owcr_l" -> add (Read_revoke (q_owcr, long_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_owcr_ul" -> add (Read_invoke (q_owcr, ulong_type))
          | "ceal_read_revinv_owcr_ul" -> add (Read_revinv (q_owcr, ulong_type))
          | "ceal_read_revoke_owcr_ul" -> add (Read_revoke (q_owcr, ulong_type))
          (* Read -- Floats *)
          | "ceal_read_invoke_owcr_f" -> add (Read_invoke (q_owcr, float_type))
          | "ceal_read_revinv_owcr_f" -> add (Read_revinv (q_owcr, float_type))
          | "ceal_read_revoke_owcr_f" -> add (Read_revoke (q_owcr, float_type))
          (* Read -- Doubles *)
          | "ceal_read_invoke_owcr_d" -> add (Read_invoke (q_owcr, double_type))
          | "ceal_read_revinv_owcr_d" -> add (Read_revinv (q_owcr, double_type))
          | "ceal_read_revoke_owcr_d" -> add (Read_revoke (q_owcr, double_type))

          (* Writes for OWCR Memory *)
          (*                        *)
          (* Write -- Pointers *)
          | "ceal_write_invoke_owcr_p" -> add (Write_invoke (q_owcr, void_ptr_type))
          | "ceal_write_revinv_owcr_p" -> add (Write_revinv (q_owcr, void_ptr_type))
          | "ceal_write_revoke_owcr_p" -> add (Write_revoke (q_owcr, void_ptr_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_owcr_i" -> add (Write_invoke (q_owcr, int_type))
          | "ceal_write_revinv_owcr_i" -> add (Write_revinv (q_owcr, int_type))
          | "ceal_write_revoke_owcr_i" -> add (Write_revoke (q_owcr, int_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_owcr_ui" -> add (Write_invoke (q_owcr, uint_type))
          | "ceal_write_revinv_owcr_ui" -> add (Write_revinv (q_owcr, uint_type))
          | "ceal_write_revoke_owcr_ui" -> add (Write_revoke (q_owcr, uint_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_owcr_l" -> add (Write_invoke (q_owcr, long_type))
          | "ceal_write_revinv_owcr_l" -> add (Write_revinv (q_owcr, long_type))
          | "ceal_write_revoke_owcr_l" -> add (Write_revoke (q_owcr, long_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_owcr_ul" -> add (Write_invoke (q_owcr, ulong_type))
          | "ceal_write_revinv_owcr_ul" -> add (Write_revinv (q_owcr, ulong_type))
          | "ceal_write_revoke_owcr_ul" -> add (Write_revoke (q_owcr, ulong_type))
          (* Write -- Floats *)
          | "ceal_write_invoke_owcr_f" -> add (Write_invoke (q_owcr, float_type))
          | "ceal_write_revinv_owcr_f" -> add (Write_revinv (q_owcr, float_type))
          | "ceal_write_revoke_owcr_f" -> add (Write_revoke (q_owcr, float_type))
          (* Write -- Doubles *)
          | "ceal_write_invoke_owcr_d" -> add (Write_invoke (q_owcr, double_type))
          | "ceal_write_revinv_owcr_d" -> add (Write_revinv (q_owcr, double_type))
          | "ceal_write_revoke_owcr_d" -> add (Write_revoke (q_owcr, double_type))
              
          (* Reads for RING Memory *)
          (*                       *)
          (* Read -- Pointers *)
          | "ceal_read_invoke_ring_p" -> add (Read_invoke (q_ring, void_ptr_type))
          | "ceal_read_revinv_ring_p" -> add (Read_revinv (q_ring, void_ptr_type))
          | "ceal_read_revoke_ring_p" -> add (Read_revoke (q_ring, void_ptr_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_ring_i" -> add (Read_invoke (q_ring, int_type))
          | "ceal_read_revinv_ring_i" -> add (Read_revinv (q_ring, int_type))
          | "ceal_read_revoke_ring_i" -> add (Read_revoke (q_ring, int_type))
          (* Read -- Integers *)
          | "ceal_read_invoke_ring_ui" -> add (Read_invoke (q_ring, uint_type))
          | "ceal_read_revinv_ring_ui" -> add (Read_revinv (q_ring, uint_type))
          | "ceal_read_revoke_ring_ui" -> add (Read_revoke (q_ring, uint_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_ring_l" -> add (Read_invoke (q_ring, long_type))
          | "ceal_read_revinv_ring_l" -> add (Read_revinv (q_ring, long_type))
          | "ceal_read_revoke_ring_l" -> add (Read_revoke (q_ring, long_type))
          (* Read -- Longs *)
          | "ceal_read_invoke_ring_ul" -> add (Read_invoke (q_ring, ulong_type))
          | "ceal_read_revinv_ring_ul" -> add (Read_revinv (q_ring, ulong_type))
          | "ceal_read_revoke_ring_ul" -> add (Read_revoke (q_ring, ulong_type))
          (* Read -- Floats *)
          | "ceal_read_invoke_ring_f" -> add (Read_invoke (q_ring, float_type))
          | "ceal_read_revinv_ring_f" -> add (Read_revinv (q_ring, float_type))
          | "ceal_read_revoke_ring_f" -> add (Read_revoke (q_ring, float_type))
          (* Read -- Doubles *)
          | "ceal_read_invoke_ring_d" -> add (Read_invoke (q_ring, double_type))
          | "ceal_read_revinv_ring_d" -> add (Read_revinv (q_ring, double_type))
          | "ceal_read_revoke_ring_d" -> add (Read_revoke (q_ring, double_type))

          (* Writes for RING Memory *)
          (*                        *)
          (* Write -- Pointers *)
          | "ceal_write_invoke_ring_p" -> add (Write_invoke (q_ring, void_ptr_type))
          | "ceal_write_revinv_ring_p" -> add (Write_revinv (q_ring, void_ptr_type))
          | "ceal_write_revoke_ring_p" -> add (Write_revoke (q_ring, void_ptr_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_ring_i" -> add (Write_invoke (q_ring, int_type))
          | "ceal_write_revinv_ring_i" -> add (Write_revinv (q_ring, int_type))
          | "ceal_write_revoke_ring_i" -> add (Write_revoke (q_ring, int_type))
          (* Write -- Integers *)
          | "ceal_write_invoke_ring_ui" -> add (Write_invoke (q_ring, uint_type))
          | "ceal_write_revinv_ring_ui" -> add (Write_revinv (q_ring, uint_type))
          | "ceal_write_revoke_ring_ui" -> add (Write_revoke (q_ring, uint_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_ring_l" -> add (Write_invoke (q_ring, long_type))
          | "ceal_write_revinv_ring_l" -> add (Write_revinv (q_ring, long_type))
          | "ceal_write_revoke_ring_l" -> add (Write_revoke (q_ring, long_type))
          (* Write -- Longs *)
          | "ceal_write_invoke_ring_ul" -> add (Write_invoke (q_ring, ulong_type))
          | "ceal_write_revinv_ring_ul" -> add (Write_revinv (q_ring, ulong_type))
          | "ceal_write_revoke_ring_ul" -> add (Write_revoke (q_ring, ulong_type))
          (* Write -- Floats *)
          | "ceal_write_invoke_ring_f" -> add (Write_invoke (q_ring, float_type))
          | "ceal_write_revinv_ring_f" -> add (Write_revinv (q_ring, float_type))
          | "ceal_write_revoke_ring_f" -> add (Write_revoke (q_ring, float_type))
          (* Write -- Doubles *)
          | "ceal_write_invoke_ring_d" -> add (Write_invoke (q_ring, double_type))
          | "ceal_write_revinv_ring_d" -> add (Write_revinv (q_ring, double_type))
          | "ceal_write_revoke_ring_d" -> add (Write_revoke (q_ring, double_type))
              
          (* TV Signals *)
          | name when ( 
              try (suffix_after "ceal_tvsig_" name) <> "" with Not_found -> false 
            ) -> 
              let suffix = suffix_after "ceal_tvsig_" name in
              (* Printf.printf "tvsig(%s)\n" suffix ;  *)
              add (Tv_signal(suffix))

          | _ -> ()       

        end (* Cil.GVarDecl case *)      
          
      | _ -> ()
        
    end 
    end (* Cil.iterGlobals *)
  end (* end init function *)    
end (* module Low *)
  
(* Foreign-Function Interface.
   
   CEAL code can call foreign C code directly, though this may not
   always result in the correct semantics.  In particular, this may
   generally lead to memory leaks or other inconsistent behavior that
   results from the foreign C code being unaware of the change
   propagation algorithm (and vice versa).  This interface helps to
   cure this problem, assuming that the foreign code provides an
   explicit way to "undo" itself.

   This iterface can be used by programmers to author libraries that
   expose:

   -- Retroactive functions: those functions that can be both invoked
   and then later revoked.

   -- Traceable data types: abstract data types bundled with some
   operations for creation and modification.  When the operations are
   revoked, they may also invalidate portions of the trace
   corresponding to other currently-invoked operations that are
   affected.  

   To avoid memory leaks, traceable data types can also clean up after
   themselves automatically (from the client's point of view) in up to
   two ways: 1) each opeations' invocation can utilize unboxed storage
   in the trace, to which it is automatically passed a pointer.  The
   type and size of this storage is automatically determined by the
   parameter's type (and must be known statically).  2) when a
   creation operation is revoked by change propagation, the data
   structure can clean itself up, in the style of a destructor
   function in C++.
*)
module Ffi = struct
  include Abbrev

  type flags = {
    wakeup : bool ; (* Can this operation "wake-up" the code that calls it? *)
    nondet : bool ; (* Does this operation return something non-deterministically? *)
  }

  (* Wakeable | Non-determinsitic | Canonical Example        *)
  (* - - - -  | - - - - - - - - - | - - - - - - - - - - - -  *)
  (* No       | No                | Write                    *)
  (* No       | Yes               | (Unstealable) Allocation *)
  (* Yes      | No                | Read                     *)
  (* Yes      | Yes               | (Stealable) Allocation   *)

  type op = { 
    srcvar  : var ;       (* Source-level variable (what's in the header file) *)
    flags   : flags ;     (* Flags explained above *)
    payload : typ option; (* The payload stored in the trace node for this op *)
    invoke  : var option; (* Library implementation of invoke(srcvar, ...) *)
    revinv  : var option; (* Library implementation of revinv(srcvar, ...) *)
    revoke  : var option; (* Library implementation of revoke(srcvar, ...) *)
  }

  (* The "view" of the operation is one of four:  *)
  type view = 
    | Srcvar (* pre-lowered code. *)
    | Invoke (* post-lowered code. *)
    | Revinv (* post-lowered code. *)
    | Revoke (* post-lowered code. *)

  let string_of_view : view -> string = 
    function
      | Srcvar -> "srcvar"
      | Invoke -> "invoke"
      | Revinv -> "revinv"
      | Revoke -> "revoke"

  let resolve : op -> view -> var = fun op view ->
    match view, (op.invoke, op.revinv, op.revoke) with
      | Srcvar, _            -> op.srcvar
      | Invoke, (Some v,_,_) -> v
      | Revinv, (_,Some v,_) -> v
      | Revoke, (_,_,Some v) -> v
      | _ -> failwith 
          (Printf.sprintf "Missing `%s' view of operation: `%s'"
             (string_of_view view) (string_of_var op.srcvar))
      
  (* Convert CIL pragma's into operations during the walk over the
     [Cil.global]s in a [Cil.file] *)
  module From_cil : sig
    type env
      
    val empty   : env
    val equal   : env -> env -> bool
    val pragma  : Cil.attribute -> env -> env
    val var     : var           -> env -> env
    val check   : var           -> env -> bool
    val resolve : var           -> env -> op 
    
  end
    = 
  struct

    module Varmap = Varinfo.Map
    module Strmap = Map.Make(String)
      
    type env = { version : int ;
                 op_env  : op  Varmap.t ;
                 var_env : var Strmap.t }

    let empty = { version = 0 ;
                  op_env  = Varmap.empty ;
                  var_env = Strmap.empty }
      
    let equal env1 env2 = (env1.version = env2.version)

    let lookup_var name env =
      try 
        Strmap.find name env.var_env 
      with
        | Not_found -> 
            (* A Note: If this happens when the prototype is actually
               present in the input, consider running cilly with
               --keepunused.  Cilly will otherwise remove any unused
               prototypes before we get a chance to see them. *)
            ignore (Cil.error "CEAL_ffi: no prototype for: %s" name) ;
            (Cil.emptyFunction name).Cil.svar

    let extend var_name options = 
      begin fun env ->
        let operation : op = 
          { srcvar = lookup_var var_name env ;
            flags  = { wakeup = false ;
                       nondet = false } ;
            payload = None ;
            invoke  = None ;
            revinv  = None ;
            revoke  = None }
        in
        let lookup_var var = 
          lookup_var var env 
        in
        let choose_payload current_payload var =
          let payload_from_arrowtype = 
            match Annot.arrow_type_unpack var.Cil.vtype with
              | _, (_::Cil.TPtr(t, _)::_), _ -> Some t
              | _ -> 
                  ignore (Cil.error "CEAL_ffi: missing payload type for: %s\n" var.Cil.vname) ;
                  None
          in
          match current_payload, payload_from_arrowtype with
            | None,    None     -> None
            | Some t,  None     -> Some t
            | None,    Some t   -> Some t
                
            (* The payload types agree *)
            | Some t1, Some t2 when (Cil.typeSig t1) = (Cil.typeSig t2) -> 
                Some t1
                  
            (* The payload types do not agree *)
            | Some t1, Some _   ->
                ignore (Cil.error "conflicting payload type: %s\n" var.Cil.vname) ;
                Some t1
        in
        (* Each attribute iteratively "refines" the operation being defined: *)
        let rec refine_operation op : Cil.attrparam list -> op = function
          | attrparam :: options -> 
              refine_operation begin match attrparam with
                | Cil.ACons("update", []) -> 
                    { op with flags = { op.flags with wakeup = true } }
                      
                | Cil.ACons("nondet", []) -> 
                    { op with flags = { op.flags with nondet = true } }
                      
                | ap -> (* TODO -- an error message *)
                    failwith (Printf.sprintf "unexpected: %s" 
                                (string_of_attrparam ap))
              end options
                
          | [] -> 
              let payload = op.payload in
              
              (* Look for invoke function. *)
              let invoke_var = lookup_var (var_name^"_invoke") in
              let payload    = choose_payload payload invoke_var in
              
              (* Look for revoke function. *)
              let revoke_var = lookup_var (var_name^"_revoke") in
              let payload    = choose_payload payload revoke_var in
              
              (* Look for revinv function. *)
              let revinv_var = lookup_var (var_name^"_revinv") in
              let payload    = choose_payload payload revinv_var in
              
              { op with 
                  invoke = Some invoke_var ; 
                  revinv = Some revinv_var ;
                  revoke = Some revoke_var ;
                  payload = payload        }
                
        in
        let operation = refine_operation operation options in
        { env with 
            version = env.version + 1 ;
            op_env = Varmap.add operation.srcvar operation env.op_env
        }
      end

    let pragma : Cil.attribute -> env -> env = function 
        (* TODO -- eventually pick one of these and eliminate the other: *)
        ( (Cil.Attr("ceal_traced_c", (Cil.ACons (var_name, [])) :: options))
        | (Cil.Attr("ceal_ffi", (Cil.ACons (var_name, [])) :: options)) ) ->
          ( extend var_name options )

      | _ -> fun env -> env

    let var v env = 
      (*Printf.eprintf "added: %s\n%!" v.Cil.vname ;*)
      (* add the variable. *)
      let env = 
        { env with var_env = 
            Strmap.add v.Cil.vname v env.var_env } 
      in      
      let attr = 
        function            
            ( (Cil.Attr("ceal_traced_c", options))
            | (Cil.Attr("ceal_ffi", options)) ) -> ( extend v.Cil.vname options )
          | _ -> ( fun env -> env )
      in              
      (* process all of its attributes *)
      List.fold_right attr v.Cil.vattr env
      
    let check : var -> env -> bool =
      fun var env -> Varmap.mem var env.op_env

    let resolve : var -> env -> op =
      fun var env -> Varmap.find var env.op_env      
  end
    
  module Strip = struct              
    class visitor = object(self)
      inherit Cil.nopCilVisitor
      method vattr (attr:Cil.attribute) = 
        match attr with
          | Cil.Attr ("ceal_traced_c", _) -> Cil.ChangeTo []
          | Cil.Attr ("ceal_ffi", _) -> Cil.ChangeTo []
          | _ -> Cil.ChangeTo [attr]
    end        
      
    let process_file (file:Cil.file) = 
      let visitor = new visitor in
      Cil.visitCilFile visitor file        
  end (* Strip module. *)
end
  


(* Zipper Intermediate Representation (Zipcfg) --
   
   -- The Zipcfg module represents programs in "functional SSA" form.
   
   -- Some aspects of C cannot be purified by the SSA form though,
   namely, memory effects (including both heap memory as well as stack
   variables whose addresses are taken and updated indirectly).
   
   -- The Zipcfg rep is itself "functional" (i.e., applicative)
   
   -- We call the representation a "zipper" since our method for
   making the representation applicative is derived from the
   Ramsey-Dias IR for C--, which is itself based on Huet's Zipper [See
   "An Applicative Control-Flow Graph Based on Huet's Zipper" by
   Ramsey & Dias].
*)
module Zipcfg = struct
  include Abbrev
    
  (* Properties of a program point. *)
  module Property = struct

    type t = 
      | Root_entry
      | Undo_code
      | Memo_code
          
    let has : t -> t Program_point.t -> bool = 
      (fun prop ppt -> List.mem prop (Program_point.props ppt))
  end

  (* Each program point carries a list of properties defined by the
     Property module above. *)

  (* sets and mappings over Program Points *)
  module Ppt = struct
    include Program_point
    include OrdThing (
      struct type t = Property.t Program_point.t
             let compare = Program_point.compare 
      end)
  end

  type ppt = Property.t Ppt.t

  type simp = 
    | Pure   of pure        (* a pure expression (no memory effects) *)
    | Read   of lval        (* a (traced) read *)
    | Alloc  of typ         (* an allocation *)
    | Peek   of lval        (* an untraced read *)
    | Poke   of lval * pure (* an untraced write *)
    | Scope                 (* creation of a fresh scope *)
    | Low    of Low.op * pure list (* A low-level operation *)
    | Clib   of pure * pure list (* a call to conventional C library code *)
    | Ffi    of Ffi.view * Ffi.op * pure list (* Foreign-function call *)

  module Rep = struct    
    (* First node -- either an unlabeled node (Entry), 
       or a node that consists of a label and some other info.
       
       This other info is: other labels (perhaps used in the source
       program, but not semantically meaningful within the IR), zero or
       more formal arguments (i.e., phi variables), and a location.  
    *)
    type label_info = label * label list * var list * ppt

    type first = 
      | Entry
      | Label of label_info
          
    (* Middle node -- internal nodes in a block with fallthrough-style control *)
    type middle = 
      | Let_block of (label list) * ppt
          (* Let_block: mutually-recursive let-binding of nested
             blocks.  Their bodies are stored off to the side (See
             Blocks module below).  *)
          
      | Let_simp  of var   * simp * ppt
      | Write     of lval  * pure * ppt
      | Cut       of label * memo * ppt
      | Memo      of scope * ppt
      | Update    of ppt
      | Rvars     of var list * ppt

    (* Memo descriptor -- should we memoize this cut point? *)
    and memo = 
      | Memo_yes of ppt * scope (* cut point is memozied *)
      | Memo_no                 (* cut point is not memoized *)
        
    (* Scope descriptor -- should we create a fresh scope? *)
    and scope = 
      | Scope_same           (* memo body uses same scope as outter context*)
      | Scope_change of pure (* memo body under a fresh scope *)
        
    (* Last node -- control transfer out of current block *)
    type last = 
      | Exit                                   
      | Cond   of pure * branch * branch * ppt  (* conditional block application *)
      | Branch of branch * ppt                  (* block application *)
      | Call   of mode * pure * pure list * ppt (* call a function *)
      | Return of pure list * ppt               (* return from current function/cut-body *)
          
    (* Mode descriptor -- should we trace the call's execution? *)
    and mode = 
      | Default  (* preserve current tracing policy *)
      | Run_core (* trace the call (even if the calling context isn't traced). *)

    (* The application of a block to a sequence of actuals 
       -- a sort of parameterized goto. *)          
    and branch = Br of label * pure list

    (* zipper block -- a block with a focus (focus is set between
       nodes of head & tail) *)
    type zblock = head * tail   
        
    (* head & tail -- represent looking backward or forward in the
       block (respectively) from the focus point that they define together. *)
    and head = First of first | Head of head * middle
    and tail = Last  of last  | Tail of middle * tail
      
    (* block -- a zblock where focus is always on the edge out of
       first node (this is just a convenient convention) *)
    type block = first * tail        

    let entry_label = Temps.entry_label

    let label_of_first = function 
      | Entry -> entry_label 
      | Label(label, _, _, _) -> label

    let label_of_block (first, tail) = label_of_first first

    let rec ht_to_first head tail = match head with
      | First first -> (first, tail)
      | Head (h, m) -> ht_to_first h (Tail (m, tail))

    let rec ht_to_last head tail = match tail with
      | Last last -> (head, last)
      | Tail (m, t) -> ht_to_last (Head(head, m)) tail

    let ht_to_first (head, tail) = ht_to_first head tail
    let ht_to_last  (head, tail) = ht_to_last  head tail
      
    let zip = ht_to_first

    let unzip (first, tail) = (First first, tail)
      
    let rec last_of_tail = function 
      | Last last -> last
      | Tail (_, tail) -> last_of_tail tail

    let last (head, tail) = last_of_tail tail

    let ppt_of_first = function
      | Entry                -> None
      | Label (_, _, _, ppt) -> Some ppt
          
    let ppt_of_middle = function
      | Let_block (_, ppt)    -> ppt
      | Let_simp  (_, _, ppt) -> ppt
      | Write     (_, _, ppt) -> ppt
      | Cut       (_, _, ppt) -> ppt
      | Memo      (_, ppt)    -> ppt
      | Update    ppt         -> ppt
      | Rvars     (_, ppt)    -> ppt
          
    let ppt_of_last = function
      | Exit                -> None
      | Cond (_, _, _, ppt) -> Some ppt
      | Branch (_, ppt)     -> Some ppt
      | Call (_, _, _, ppt) -> Some ppt
      | Return (_, ppt)     -> Some ppt         

  end (* Rep module *) 
  
  include Rep

  (* Blocks -- A particular kind of mapping from labels to blocks.
     We'll use this mapping both to store the blocks when they aren't
     zipped open and also to resolve indirect block references in the
     graph (i.e., block labels). The interface enforces various
     uniqueness criteria: blocks must be explicitly removed before
     being inserted again (double-inserts will yeild exception
     [Duplicate]).  Similarly, we enforce that instances be disjoint
     for the [union] operation.  *)
  module Blocks : sig 
    type t
    exception Duplicate of label

    val empty    : t
    val labels   : t -> label list
    val labels'  : t -> (label * ppt option) list
    val find     : label -> t -> block
    val insert   : block -> t -> t
    val focus    : label -> t -> block * t
    val focusp   : (block -> bool) -> t -> block * t
    val union    : t -> t -> t        
  end        
    = 
  struct
    module M = Label.Map    
    type t = block M.t
    exception Duplicate of label
    
    let empty = M.empty      

    let labels blocks = M.fold (fun label _ labels -> label :: labels) blocks []
    
    let labels' blocks = M.fold 
      (fun label (first,_) labels -> 
         (label, ppt_of_first first) :: labels) blocks []

    let find label blocks = M.find label blocks      

    let insert block blocks = 
      let block_label = label_of_block block in
      match (block_label) with 
        | Cil.Label _ -> 
            if M.mem block_label blocks then raise (Duplicate block_label)
            else M.add block_label block blocks
        | _ -> raise (Invalid_argument "Blocks.insert: label is not 'real'")
            
    let focus label blocks = (find label blocks, M.remove label blocks)

    let focusp pred blocks = 
      let scan label block (yes, no) = 
        match (yes : block option) with
          | None when pred block -> (Some block, no)
          | _ -> (yes, M.add label block no) in
      match (M.fold scan blocks (None, empty)) with
        | (Some block, blocks) -> (block, blocks)
        | (None, _) -> raise Not_found
            
    let union  blocks_1 blocks_2 = 
      M.fold (fun _ block blocks -> insert block blocks) 
        blocks_1 blocks_2

  end (* Blocks module *)

  (* The type of a Zipcfg instance is [zt] when its zipped open (i.e.,
     when it has a focus) and [t] when it has no focus.
     
     Concretely [zt] is a pair: a [zblock] and a mapping of type [Blocks.t] 
  *)
  type t     = Blocks.t
  type zt    = zblock * Blocks.t
  type nodes = zt -> zt
      
  let emptyz : zt = (First Entry, Last Exit), Blocks.empty
  let empty  : t  = Blocks.insert (Entry, Last Exit) Blocks.empty

  let labels blocks = Blocks.labels blocks
    
  let focus label blocks = 
    let (block, blocks) = Blocks.focus label blocks in
    (unzip block, blocks)

  let entry blocks = match focus entry_label blocks with
    | ((First Entry, tail), blocks) -> ((First Entry, tail), blocks)
    | _ -> failwith "non-entry block has entry_label"

  let exit blocks = 
    let is_exit block = match last (unzip block) with Exit -> true | _ -> false in
    let (block, blocks) = Blocks.focusp is_exit blocks in
    let (head, last) = ht_to_last (unzip block) in
    ((head, Last last), blocks)

  let drop_head ((head,tail), blocks) = ((First Entry, tail), blocks)
      
  let unfocus (zblock, blocks) = Blocks.insert (zip zblock) blocks
   
  let info_of_label label blocks = match focus label blocks with
    | (First (Label info), _), _ -> info
    | _                          -> failwith (Printf.sprintf "no info for label: %s" (string_of_label label))

  let formals_of_label label blocks = 
    match info_of_label label blocks with
      | (_,_,formals,_) -> formals

  let ppt_of_label label blocks =
    match info_of_label label blocks with
      | (_,_,_,ppt) -> ppt
  
  let merge_nodes nodes1 nodes2 : nodes = 
    fun z ->
      let (zblock, blocks) = nodes1 z in
      let blocks = Blocks.union (unfocus (nodes2 emptyz)) blocks in
      (zblock, blocks)

  let merge_empty nodes1 nodes2 : nodes =
    if nodes2 emptyz = emptyz 
    then nodes1
    else merge_nodes nodes1 nodes2

  let nodes_of_graph graph : nodes = fun z -> 
    begin assert (z = emptyz) ; entry graph end

  let label_of_ppt : t -> ppt -> label option = fun blocks ->
    let map = 
      List.fold_right 
        ( function 
            | ( label, Some ppt ) -> Ppt.Map.add ppt label
            | ( _    , None     ) -> (fun map -> map) 
        )
        (Blocks.labels' blocks) Ppt.Map.empty 
    in
    (fun ppt -> try Some ( Ppt.Map.find ppt map ) with Not_found -> None)
    

  module Build : sig
    (* All the funtions return type [nodes].  Each value is itself a
       function of type [nodes = zt -> zt].  Each such function has a
       uniform behavior: it inserts some nodes at the current focus,
       leaving the focus just before what's been inserted.

       The main benefit of this behavioral convention is the
       convenient, idomatic code that follows from it.  The
       constructors can be composed as combinators to produce
       constructors for larger and larger fragments of graphs (See
       [From_cil] and [Dest_passing] modules for concrete examples of
       this usage).  *)

    exception Dropped_head of head (* you (unknowingly?) dropped a head *)
    exception Dropped_tail of tail (* you (unknowingly?) dropped a tail *)

    val cons_id     : nodes
    val cons_first  : first -> nodes      
    
    val no_qualty : var -> var

    (* Nest mutually-recursive blocks within the current one *)    
    val let_blocks  : ppt -> (label_info * nodes) list  -> nodes
    val let_blocks' : ppt -> (label_info * t    ) list  -> nodes
    val let_blocks'': ppt -> label list                 -> nodes

    (* Nest a single block within the current one *)
    val let_block   : ppt -> label -> var list -> nodes -> nodes
    val let_block'  : ppt -> label -> var list -> t     -> nodes
      
    (* Other middle nodes *)
    val cons_middle : middle -> nodes
    val cut         : ppt -> label -> memo -> nodes
    val memo        : ppt -> scope -> nodes
    val update      : ppt -> nodes
    val rvars       : ppt -> vars  -> nodes
    val let_simp    : ppt -> var   -> simp -> nodes
    val write       : ppt -> lval  -> pure -> nodes
    val poke        : ppt -> lval  -> pure -> nodes
      
    (* Last nodes *)
    val cons_last   : last -> nodes
    val branch      : ppt -> branch -> nodes
    val cond        : ppt -> pure -> branch -> branch -> nodes
    val call        : ppt -> mode -> pure -> pure list -> nodes
    val return      : ppt -> pure list -> nodes
  end 
    = 
  struct      
    let ( **> ) f x = f x
      
    let no_qualty : var -> var = fun v ->
      (* SIDE-EFFECT: We make the blanket assumption that no variable
         carries a qualifier on its (outer) type. *)
      ( ( v.Cil.vtype <- (Qual.type_without_qual v.Cil.vtype) ) ; v )

    exception Dropped_tail of tail
    exception Dropped_head of head

    (* think of this as a "consumer" of unreachable code.
       See [cons_last] for an example of its use. *)
    let unreachable_tail = function
      | Last Exit -> ()
      | t         -> raise (Dropped_tail t)

    let unreachable_head = function
      | First Entry -> ()
      | h           -> raise (Dropped_head h)

    let cons_id = fun z -> z 

    let cons_first (f:first) = fun ((h,t), blocks) -> 
      unreachable_head h; ((First f, t), blocks)

    let cons_middle (m:middle) = 
      fun ((h,t), blocks) -> ((h, Tail (m, t)), blocks)

    let cons_last (last:last) = fun ((h, t), blocks) ->
      unreachable_tail t; ((h, Last last), blocks)

    let let_blocks'' ppt = function
      | []     -> cons_id
      | labels -> cons_middle (Let_block(labels, Ppt.from ppt))

    let let_blocks' ppt specs = fun ((head, tail), blocks) ->
      let labels = List.map (fun ((x,_,_,_), _) -> x) specs in
      let mk_block ((label, others, vars, pp), g) =
        match entry g with ((First Entry, tail), blocks) -> 
          let vars = List.map no_qualty vars in
          unfocus ((First(Label(label,others,vars,pp)), tail), blocks)
          | _ -> failwith "impossible" in
      if true then (* TODO: the true case should replace the false case *)
        let blocks = List.fold_left Blocks.union blocks (List.map mk_block specs) in
        let_blocks'' ppt labels ((head, tail), blocks)
      else
        ((head, Tail(Let_block(labels, Ppt.from ppt), tail)),
         List.fold_left Blocks.union blocks (List.map mk_block specs))
        
    let let_block' ppt lab vars graph =
      let info = (lab, [], List.map no_qualty vars, Ppt.from ppt) in
      let_blocks' ppt [(info, graph)] 

    let let_blocks ppt specs =
      let fs     = List.map  (fun (_,f)   -> f) specs in
      let graphs = List.map  (fun f       -> unfocus (f emptyz)) fs in
      let specs' = List.map2 (fun (i,_) g -> (i,g)) specs graphs in
      let_blocks' ppt specs'

    let let_block ppt lab vars nodes =      
      let info = (lab, [], List.map no_qualty vars, ppt) in
      let_blocks ppt [(info, nodes)] 
        
    let cut ppt label memo     = cons_middle (Cut(label, memo, Ppt.from ppt))
    let memo ppt scope         = cons_middle (Memo(scope, Ppt.from ppt))
    let update ppt             = cons_middle (Update(Ppt.from ppt))
    let rvars ppt rvars        = cons_middle (Rvars(List.map no_qualty rvars, Ppt.from ppt))
    let let_simp ppt var simp  = cons_middle (Let_simp(no_qualty var, simp, Ppt.from ppt))
    let write ppt lval pure    = cons_middle (Write(lval, pure, Ppt.from ppt))
    let poke ppt lval pure     = let_simp ppt Temps.wildcard_var (Poke(lval, pure))
      
    let branch ppt branch        = cons_last (Branch (branch, Ppt.from ppt))
    let call ppt mode pure pures = cons_last (Call (mode, pure, pures, Ppt.from ppt))
    let return ppt pures         = cons_last (Return (pures, Ppt.from ppt))
    let cond ppt pure br_ifso br_ifnot = 
      cons_last (Cond (pure, br_ifso, br_ifnot, Ppt.from ppt))      
        
  end (* Build module *)

  
  (* (**) Converting from CIL into our intermediate representation (IR)

     We convert a function by processing its dominator tree, where
     each node of the tree corresonds to a statement of the function.
     
  (**) The Algorithm:

     Inputs:      
     -- A statement-to-label mapping sl.
     -- A variable substitution (mapping variables to new SSA variable versions).
     -- A dominator tree (containing a statement and a list of children at each node).
     
     Output:
     -- An IR constructor that constructs a subgraph for the given
        dominator subtree.

     
     Overview of Steps:

     -- We compute an IR constructor [cons_stmt] for the current
     statement which constructs some kind of IR node.  Based on the
     stmt/node kind and its successors, we choose how to handle the
     sucessors to the node, as well as the children of the stmt in the
     dominator tree.
     
     -- Of the children in the dominator tree, at most one can be
     expressed with fall-through control (see below).  For the rest we
     compute an IR constructor [cons_let_children] that begins with a
     mutually-recursive Let_block binding that contains the labels for
     these children.  It also recursively constructs the blocks for
     each child.

     -- Finally, compose the results and possibly recur on any
     fall-through stmt/node.
     

  (**) Choosing an IR fallthrough edge
     
     Given a current statement in the dominator tree, the selection of
     a fallthrough edge/statement dictates how to process the children
     (in the dominator tree) of the current one.  In particular, each
     non-fallthrough child will be Let_block-bound syntatically
     _before_ the current statement, and the fallthrough child, if it
     exists and we can express it as such, will be placed syntatically
     _after_ the node for the current statement.

     Can we "express it as such"? Only if the fallthrough statement
     has precisely one predecessor: the current statement.  If it has
     multiple predecessors it will need a label, and in our IR,
     fallthrough into a label is _not_ expressable (and thank goodness
     for that!  By contrast, note that this _is_ expressible in both C
     and in Cil).

     Our procedure for choosing a fallthrough edge has two steps:
     first we consider the [skind] field of the current statement to
     see if the statement form gives rise to a possible fallthrough
     statement.  If it does, we check whether this canidate
     fallthrough statement has multiple predecessors, and if not, we
     can express this Cil fallthrough edge as an IR fallthrough edge.
  *)

  module From_cil : sig
    val fundec : Ffi.From_cil.env -> Cil.fundec -> nodes
  end
    =
  struct
    module DT = Cil_static_analysis.Dom_tree
    module DF = Cil_static_analysis.Dom_frontier
    module PF = Cil_static_analysis.Phi_funs
    module LS = Cil_static_analysis.Live_sets
    module Ppt = Program_point

    (* statement labels *)
    module SL = Stmt.Map
    type   sl = label SL.t
    
    module B = Build
    module R = Rep              

    let ( **> ) f x = f x
      
    let stmt_loc (stmt:Cil.stmt) = (Cil.get_stmtLoc stmt.skind)

    (* Variable substitution: maps old variables to new variables and
       performs substitutions over pures and lvalues.
       
    (**) Note: We assume (but do not enforce) that the domain and
       co-domain are disjoint (i.e., that the substitution is
       idempotent).  
    *)
    module Subst = struct
      module M = Varinfo.Map
      type t = varinfo M.t
          
      let empty = M.empty
        
      let extend v sub = 
        let v' = Temps.fresh_ver v in
        M.add v v' sub
          
      let find = M.find
        
      let apply var sub =
        try find var sub 
        with Not_found -> var
          
      class visitor sub = object
        inherit nopCilVisitor
        method vvrbl (var:varinfo) = ChangeTo(apply var sub)
      end
        
      let apply_exp  exp  sub  = Cil.visitCilExpr (new visitor sub) exp
      let apply_lval lval sub  = Cil.visitCilLval (new visitor sub) lval
    end    
    
    let pure_of_var  (sub:Subst.t) (var:var)   : pure = Lval(Var(Subst.apply var sub), NoOffset)
    let pure_of_exp  (sub:Subst.t) (exp:pure)  : pure = Subst.apply_exp exp sub
    let lval_of_lval (sub:Subst.t) (lval:lval) : lval = Subst.apply_lval lval sub
    let pure_of_lval (sub:Subst.t) (lval:lval) : pure = Lval(lval_of_lval sub lval)

    let ppt_root = Ppt.root
      
    let stmt_label (sl:sl) (stmt:Cil.stmt) : (sl * label * label list) =
      if SL.mem stmt sl then 
        let label = SL.find stmt sl in
        let others = List.filter (fun label' -> label' <> label) stmt.labels in
        (sl, label, others)
      else              
        let loc = Cil.get_stmtLoc stmt.skind in
        let label, others = Temps.pick_label loc stmt.labels in
        let sl' = SL.add stmt label sl in 
        (sl', label, others)
                       
    (* compute a constructor for a (non-tail) call.  In particular, it
       returns a constructor that, when given a zgraph, it inserts a
       call before the focus.  It takes an sl and a  

       Details: This involves wrapping the focus in a
       Let_block-binding, creating a cut to this block and then,
       within the cut, performing the call (as a tail-call). *)
    let call (sub:Subst.t) (loc,fname) lval fnpure pures : Subst.t * nodes = 
      (* The calling mode, the function (as a pure) and the function (variable). *)
      let mode, fnpure, fnvar = match fnpure, Annot.kw_of_exp fnpure with
        | _, (Some (Annot.Core fnvar)) -> Run_core, (Cil.Lval (Cil.Var fnvar, Cil.NoOffset)), fnvar
        | Cil.Lval (Cil.Var fnvar, Cil.NoOffset), None -> Default, fnpure, fnvar
        | _ -> failwith "illegal call site: all calls must be direct (i.e., no function pointers)"
      in
      (* The function return type. *)
      let ret_type = match fnvar.Cil.vtype with
        | Cil.TFun(t, _, _, _) -> t
        | _                    -> assert false
      in
      let label  = Temps.fresh_label loc in
      let cons_call = 
        fun z -> B.cut (ppt_root loc (Some fname)) label Memo_no
          **> B.call (ppt_root loc (Some fname)) mode fnpure pures 
          **> z 
      in
      begin match lval, (Cil.typeSig ret_type) with
          
        (* Case 1: No lval for return; function has void return type *)
        | None, (Cil.TSBase (Cil.TVoid _)) -> 
            let label_info = (label, [], [], ppt_root loc (Some fname)) in
            sub, (fun z -> B.let_blocks' (ppt_root loc (Some fname)) [label_info, unfocus z]
                    **> cons_call **> emptyz)
              
        (* Case 2: No lval for return but function has non-void return
           type (this case applies when the return value is being
           ignored).  To make the type checking easier later on we
           introduce a dummy variable that receives the return value--
           this dummy variable will otherwise be unused. *)
        | None, _ ->
            let dum_var    = Temps.fresh_var ret_type in
            let label_info = (label, [], [dum_var], ppt_root loc (Some fname)) in
            sub, (fun z -> B.let_blocks' (ppt_root loc (Some fname)) 
                    [label_info, unfocus z]
                    **> cons_call **> emptyz)
            
        (* Case 3: Lval for return and (presumably) a non-void return type *)
        | Some(lval), _  -> 
            begin match lval with
              | (Var v, Cil.NoOffset) ->
                  let sub        = Subst.extend v sub in
                  let v'         = Subst.find v sub in
                  let label_info = (label, [], [v'], ppt_root loc (Some fname)) in
                  sub, (fun z -> B.let_blocks' (ppt_root loc (Some fname)) 
                          [label_info, unfocus z]
                          **> cons_call **> emptyz)

              | lval ->
                  let v           = Temps.fresh_var (Cil.typeOfLval lval) in
                  let label_info  = (label, [], [v], ppt_root loc (Some fname)) in
                  let cons_update = (B.write (ppt_root loc (Some fname)) 
                                       (lval_of_lval sub lval) 
                                       (pure_of_var sub v)) 
                  in
                  sub, (fun z -> B.let_blocks' (ppt_root loc (Some fname))
                          [label_info, unfocus (cons_update **> z)]
                          **> cons_call **> emptyz)
            end              
      end

    (* convenience function -- function calls *)
    let call' (sl:sl) (sub:Subst.t) (loc,fname) lval fnpure pures : sl * Subst.t * nodes =
      let (sub, cons) = call sub (loc,fname) lval fnpure pures in sl, sub, cons

    (* convenience function -- foriegn-function calls *)
    (* TODO? -- Merge this logic with the logic above for non-foriegn calls? *)
    let ffi_call (sl:sl) (sub:Subst.t) (loc,fname) lval op pures : sl * Subst.t * nodes =
      let pures = List.map (pure_of_exp sub) pures in
      let ret_type = match op.Ffi.srcvar.Cil.vtype with
        | Cil.TFun(t, _, _, _) -> t
        | _                    -> assert false
      in
      begin match lval, (Cil.typeSig ret_type) with
          
        (* Case 1: No lval for return; function has void return type *)        
        | None, (Cil.TSBase (Cil.TVoid _)) ->
            sl, sub, B.let_simp (ppt_root loc (Some fname)) 
              Temps.wildcard_var (Ffi (Ffi.Srcvar, op, pures))
              
        (* Case 2: No lval for return but function has non-void return
           type (this case applies when the return value is being
           ignored).  To make the type checking easier later on we
           introduce a dummy variable that receives the return value--
           this dummy variable will otherwise be unused. *)            
        | None, _ ->
            let dum_var = Temps.fresh_var ret_type in
            sl, sub, B.let_simp (ppt_root loc (Some fname)) 
              dum_var (Ffi (Ffi.Srcvar, op, pures))

        (* Case 3: Lval for return and (presumably) a non-void return type *)
        | Some(lval), _ ->
            begin match lval with
              | (Var v, Cil.NoOffset) ->
                  let sub = Subst.extend v sub in
                  let v'  = Subst.find   v sub in
                  sl, sub, B.let_simp (ppt_root loc (Some fname)) 
                    v' (Ffi (Ffi.Srcvar, op, pures))

              | lval ->
                  let v = Temps.fresh_var (Cil.typeOfLval lval) in
                  sl, sub, fun z -> 
                    begin B.let_simp (ppt_root loc (Some fname)) v (Ffi (Ffi.Srcvar, op, pures))
                      **> B.write    (ppt_root loc (Some fname)) (lval_of_lval sub lval) (pure_of_var sub v)
                      **> z
                    end
            end
      end
            

    (* Compute the dominator tree, dominator frontier, live sets and
       phi-functions for a fundec *)
    let preprocess_fundec (fundec : Cil.fundec) 
        : DT.t option * DF.t * LS.t * PF.t =

      (* First, _before_ removing duplicate branches under the IF
         statements that encode Cuts, compute liveness and
         phi-functions. *)      
      Cil.prepareCFG fundec ;
      Cil.computeCFGInfo fundec false ;
      let dom_tree = DT.compute fundec ~extra:"-a" in
      let dom_fron = DF.compute dom_tree in
      let liveness = LS.compute fundec in
      let phi_funs = PF.compute dom_fron in

      (* Next, after computing the liveness and phi-functions (see
         above), remove the duplicate branches for cuts; there isn't a
         deep reason for this, just a technical one: it makes it
         easier to parse the resulting dom-tree into abstract syntax
         for our intermediate representation *)
      Annot.Remove_duplicate_cut_branches.of_fundec fundec ;
      Cil.computeCFGInfo fundec false ;
      let dom_tree' = DT.compute ~extra:"-b" fundec in      
      (dom_tree', dom_fron, liveness, phi_funs)
      
        
    (* Create a graph constructor for a function (a Cil.fundec).
       
       As is the convention, the constructor will insert the nodes of
       the function before the given focus.  Since fundec's always
       have a return statement (which post-dominates all statements),
       the graph produced will have explicit returns (it will not have
       an Exit node).  Consequently, if the give focus has a tail
       other than Exit, a Zipcfg.Build.Unreachable exception will be
       raised when the constructor is used. *)
    let fundec (ffi_env : Ffi.From_cil.env) (fundec : Cil.fundec) =
      
      let fname = fundec.Cil.svar.Cil.vname in

      let dom_tree, dom_fron, liveness, phi_funs = 
        preprocess_fundec fundec 
      in
      
      let lval_is_foreign_c lval =
        let result =
          (Qual.lval_is_foreign_c lval) ||
            (Qual.location_is_foreign_c fundec.Cil.svar.Cil.vdecl)
        in
        (* For debugging. *)
        begin if false then
          ignore begin if result then
            log "IS  foreign C:  %s : %s\n" 
              (string_of_lval lval) 
              (string_of_type (Cil.typeOfLval lval))
          else
            log "NOT foreign C: %s : %s\n" 
              (string_of_lval lval) 
              (string_of_type (Cil.typeOfLval lval))
          end
        end ;
        result
      in

      let live_phi_vars (stmt:Cil.stmt) : (var list) =
        let join_vars = Varinfo.set_from_list (PF.get phi_funs stmt) in
        let live_vars = Varinfo.set_from_list (LS.get liveness stmt) in
        Varinfo.Set.elements (Varinfo.Set.inter join_vars live_vars)
      in

      let mk_phi_vars (sub:Subst.t) (stmt:Cil.stmt) : (Subst.t * var list) =
        let f (sub, vars_out) var = 
          let sub = Subst.extend var sub in 
          (sub, (Subst.find var sub)::vars_out) 
        in
        let sub, vars = (List.fold_left f (sub,[]) (live_phi_vars stmt)) in
        (sub, List.rev vars)
      in
      
      let phi_vars (sub:Subst.t) (stmt:Cil.stmt) : var list =
        List.map (fun var -> Subst.apply var sub) (live_phi_vars stmt)
      in

      let scope_from_annot sub = function
        | Annot.Scope_change exp -> Scope_change (pure_of_exp sub exp)
        | Annot.Scope_same       -> Scope_same
      in

      let memo_from_annot pt sub = function
        | Annot.Memo_yes scope    -> Memo_yes (pt, (scope_from_annot sub scope))
        | Annot.Memo_no           -> Memo_no
      in

      let rec cons_tree (sl:sl) (sub:Subst.t) (cut_ret_site:stmt option) = 
        let mk_br (sl:sl) (sub:Subst.t) (loc:loc) (stmt:Cil.stmt) : (sl * nodes * R.branch) =
          let sl, label, _ = stmt_label sl stmt in
          let args = List.map (pure_of_var sub) (phi_vars sub stmt) in
          let nodes, br = 
            match cut_ret_site with 
              | Some s when s.sid = stmt.sid ->
                  let label' = Temps.fresh_label loc in
                  let nodes = B.let_block (ppt_root loc (Some fname)) label' [] 
                    (B.return (ppt_root loc (Some fname)) args) in 
                  (nodes,     R.Br (label', []))
              | _ ->
                  (B.cons_id, R.Br (label, args))
          in
          sl, nodes, br
        in

        function | DT.Tree(stmt, children) -> begin
          if false then
            ignore (log "@[stmt: %d,@?idom of {%a},@?df is {%a},@?phi vars are {%a}, for:@?%a@]@!"
                      stmt.sid 
                      (Pretty.docList ~sep:(Pretty.text ",") Pretty.num) 
                      (List.map (function DT.Tree(stmt, _) -> stmt.sid) children)
                      (Pretty.docList ~sep:(Pretty.text ",") Pretty.num)
                      (List.map (function stmt -> stmt.sid) (DF.get dom_fron stmt))
                      (Pretty.docList ~sep:(Pretty.text ",") Pretty.text)
                      (List.map (function var -> var.vname) (PF.get phi_funs stmt))
                      (printStmt my_printer) stmt) 
          ;
          (* Create a constructor for the stmt, 
             compute the kind of successor we expect, if any *)
          let sl, sub, cons_stmt, 
            (succ_kind :
               [`Succ_ind          (* -- successor handled by indirection at current node *)
               |`Succ_seq of stmt  (* -- (unique) succ requires no label, no branch to get there *)
               |`Succ_br  of stmt  (* -- (unique) succ requires a branch to get there *)
               |`Succ_cut of 
                   stmt * stmt     (* -- successor, the fst stmt, is under a cut, cut returns to the snd stmt *)
                     (*|`Succ_none         (* -- no successor at all. *)*)
               ])
            =
            let succ_seqbr succ = 
              let imposs s = failwith ("mid_seq_br: "^s) in
              match succ.preds with 
                | []                       -> imposs "no preds ?"
                | [pred] when pred == stmt -> `Succ_seq succ
                | [pred]                   -> imposs "succ pred <> stmt ?"
                | _                        -> `Succ_br succ
            in              
            let succs_err (case:string) (expected:int) (succs:stmt list) = 
              Printf.sprintf "%s: %s: unexpected # of successors (expected %d, found %d)" 
                fundec.Cil.svar.Cil.vname
                case expected (List.length succs)
            in
            let unsup_err (case:string) =
              Printf.sprintf "%s: %s: this case is not supported" 
                fundec.Cil.svar.Cil.vname case
            in
            let invalid_err (case:string) =
              Printf.sprintf "%s: %s: this case must be transformed away (see Cil.prepareCFG)" 
                fundec.Cil.svar.Cil.vname case
            in
            match (stmt.skind, stmt.succs) 
            with
              | (Cil.Block _, [succ]) -> 
                  sl, sub, B.cons_id, succ_seqbr succ
                    
              | (Cil.Block _, succs)  -> 
                  failwith (succs_err "Cil.Block" 1 succs)
                    
              | (Cil.Loop(_, loc, _, _), [succ]) -> 
                  let sl, nodes, br = mk_br sl sub loc succ in
                  sl, sub, (fun z -> 
                              begin nodes
                                **> B.branch (ppt_root loc (Some fname)) br 
                                **> z end), `Succ_ind
                    
              | (Cil.Loop _, succs)              -> 
                  failwith (succs_err "Cil.Loop" 1 succs)
                    
              | (Cil.Return(expop, loc), []) ->                    
                  sl, sub, B.return (ppt_root loc (Some fname))
                    (match expop with 
                       | Some exp -> [pure_of_exp sub exp] 
                       | None -> []), `Succ_ind
                    
              | (Cil.Return _, succs) -> 
                  failwith (succs_err "Cil.Return" 0 succs)
                    
              | (Cil.Goto(stmtrf, loc), [succ]) -> 
                  if not (!stmtrf == succ) then 
                    failwith "Cil.Goto: successor is incorrect?" ;
                  let sl, nodes, br = mk_br sl sub loc !stmtrf in
                  sl, sub, (fun z -> 
                              begin nodes 
                                **> B.branch (ppt_root loc (Some fname)) br 
                                **> z end), `Succ_ind
                    
              | (Cil.Goto _, succs) ->
                  failwith (succs_err "Cil.Goto" 0 succs)
                    
              (* Cut *)
              | (Cil.If (exp, _, _, loc), [cut_ret; cut_beg]) 
                  when Annot.kw_is_cut (Annot.block_kw_of_exp exp) -> 
                  (* NOTE: pattern makes assumptions about succ list ordering *)
                  let sl, cut_label, _ = stmt_label sl cut_ret in
                  let memo = match Annot.block_kw_of_exp exp with 
                    | Some (Annot.Cut m) -> memo_from_annot (ppt_root loc (Some fname)) sub m
                    | _                  -> assert false
                  in
                  sl, sub, B.cut (ppt_root loc (Some fname)) cut_label memo, 
                    (`Succ_cut (cut_beg, cut_ret))
                    
              (* If *)
              | (Cil.If (exp, _, _, loc), [succ_ifnot; succ_ifso]) ->
                  (* NOTE: pattern makes assumptions about succ list ordering *)
                  (* !!BUG: what happens with succ_ifso or succ_ifnot are cut_ret_sites?? *)
                  let pure                 = pure_of_exp sub exp in
                  let sl, nodes1, br_ifso  = mk_br sl sub loc succ_ifso in
                  let sl, nodes2, br_ifnot = mk_br sl sub loc succ_ifnot in
                  sl, sub, 
                  (fun z -> 
                     begin nodes1 
                       **> nodes2 
                       **> B.cond (ppt_root loc (Some fname)) pure br_ifso br_ifnot 
                       **> z end), `Succ_ind
                    
              | (Cil.If _, succs) ->
                  failwith (succs_err "Cil.If" 2 succs)
                    
              | (Cil.Instr instrs, succs) ->
                  let cons_instr sl sub = function
                      
                    (* Let-bind a "simple" expression: alloc, read or pure *)
                    | Cil.Set ((Cil.Var v, Cil.NoOffset), exp, loc) -> 
                        let sub' = Subst.extend v sub in
                        let simp = match exp, Annot.kw_of_exp exp with
                          | _, Some (Annot.Alloc typ) -> Alloc typ (* TODO-TYPETRANS *)
                              
                          (* Peek / Read *) (* TODO-TYPETRANS *)
                          | Cil.Lval((Mem _, _) as lval), None  -> 
                              if lval_is_foreign_c lval then
                                Peek (lval_of_lval sub lval)
                              else
                                Read (lval_of_lval sub lval)
                                  
                          (* TODO -- check that exp is pure 
                             (no lval's with memory access) *)
                          | _, _ -> Pure (pure_of_exp sub exp)
                        in
                        sl, sub', B.let_simp (ppt_root loc (Some fname))
                          (Subst.find v sub') simp
                          
                    | Cil.Set ((Cil.Var _, _), _, _) -> assert false
                        
                    (* Poke / Write *)
                    | Cil.Set (lval, exp, loc) ->
                        (* TODO -- check that exp is pure *)
                        (* TODO-TYPETRANS *)
                        if lval_is_foreign_c lval then
                          sl, sub, B.poke (ppt_root loc (Some fname)) 
                            (lval_of_lval sub lval)
                            (pure_of_exp sub exp)
                            
                        else
                          sl, sub, B.write (ppt_root loc (Some fname)) 
                            (lval_of_lval sub lval) 
                            (pure_of_exp sub exp)
                            
                    (* Memo *)
                    | Cil.Call (None, exp, _, loc) 
                        when Annot.kw_is_memo (Annot.kw_of_exp exp) ->
                        sl, sub, B.memo (ppt_root loc (Some fname))
                          begin match Annot.kw_of_exp exp with 
                            | Some (Annot.Memo s) -> scope_from_annot sub s
                            | _                   -> assert false
                          end
                          
                    (* Scope *)
                    | Cil.Call (Some (Cil.Var v, Cil.NoOffset), exp, _, loc) 
                        when Annot.kw_is_scope (Annot.kw_of_exp exp) ->
                        let sub' = Subst.extend v sub in
                        sl, sub', B.let_simp (ppt_root loc (Some fname)) 
                          (Subst.find v sub') Scope
                          
                    (* Propagate *)
                    | Cil.Call (None, exp, _, loc) 
                        when Annot.kw_is_propagate (Annot.kw_of_exp exp) ->
                        sl, sub, B.let_simp (ppt_root loc (Some fname)) 
                          Temps.wildcard_var 
                          (Low (Low.Propagate, []))
                          
                    (* Kill *)
                    | Cil.Call (None, exp, [ptr_exp], loc) 
                        when Annot.kw_is_kill (Annot.kw_of_exp exp) ->
                        sl, sub, B.let_simp (ppt_root loc (Some fname)) 
                          Temps.wildcard_var 
                          (* TODO-TYPETRANS ? *)
                          (Low (Low.Alloc_kill, [pure_of_exp sub ptr_exp]))
                          
                    (* Ffi calls *)
                    | Cil.Call (lvalop, 
                                (Cil.Lval (Cil.Var v, Cil.NoOffset)),
                                exps, loc) 
                        when Ffi.From_cil.check v ffi_env ->
                        ffi_call sl sub (loc,fname) lvalop 
                          (Ffi.From_cil.resolve v ffi_env) exps
                          
                    (* Function calls *)
                    | Cil.Call (lvalop, fnexp, exps, loc) ->
                        let fnexp = pure_of_exp sub fnexp in
                        let exps  = List.map (pure_of_exp sub) exps in
                        call' sl sub (loc,fname) lvalop fnexp exps
                          
                    | Cil.Asm _ -> failwith (unsup_err "Cil.Asm")
                  in
                  
                  let rec cons_instrs sl sub = function
                    | [] -> sl, sub, B.cons_id
                    | instr::instrs -> 
                        let sl, sub, cons  = cons_instr  sl sub instr in
                        let sl, sub, cons' = cons_instrs sl sub instrs in
                        sl, sub, (fun z -> cons **> cons' **> z)
                  in 
                  let sl, sub, cons = cons_instrs sl sub instrs in
                  sl, sub, cons, 
                  begin match succs with 
                      (* | []     -> `Succ_none *)
                    | [succ] -> succ_seqbr succ
                    | _      -> failwith (succs_err "Cil.Instr" 1 succs)
                  end
                    
              (* We assume these have been transformed away (see Cil.prepareCFG) *)
              | (Cil.Break _, _)      -> failwith (invalid_err "Cil.Break")
              | (Cil.Continue _, _)   -> failwith (invalid_err "Cil.Break")
              | (Cil.Switch _, _)     -> failwith (invalid_err "Cil.Switch")
                  
              (* We won't support these. *)
              | (Cil.TryFinally _, _) -> failwith (unsup_err "Cil.TryFinally")
              | (Cil.TryExcept _, _)  -> failwith (unsup_err "Cil.TryExcept")
          in            
          
          (* Create a constructor for the children that need to be
             let-bound.  At most one child exists that need not be
             Let_block-bound.  This child corresponds to the
             "fallthrough statement", when it exists. *)
          let sl, cons_let_children =
            
            (* Compute constructors for child blocks (those to be Let_block-bound) *)
            let cons_let_children to_be_labeled = 
              let rec make_block_consers sl accum = function
                | [] -> sl, (List.rev accum)
                | ((DT.Tree(s,_)) as tree)::children ->
                    let sl, label, others = stmt_label sl s in 
                    let sub, vars = mk_phi_vars sub s in
                    let spec      = (label, others, vars, 
                                     ppt_root (stmt_loc s) (Some fname)) in
                    let conser    = fun sl -> (cons_tree sl sub cut_ret_site tree) in
                    make_block_consers sl ((spec, conser) :: accum) children
              in
              let sl, block_consers_0 = make_block_consers sl [] to_be_labeled in
              let block_consers = List.map (fun (spec, f) -> (spec, f sl)) block_consers_0 in
              sl, B.let_blocks (ppt_root (stmt_loc stmt) (Some fname)) block_consers
            in
            
            let children_without s = 
              let filter_fn (DT.Tree(s', _)) = s.sid != s'.sid in
              List.filter filter_fn children
            in
            
            match succ_kind with
              | `Succ_seq s -> (* assert: no Let_block-bound children possible *)
                  begin match (children_without s) with (* sanity check *)
                    | [] -> sl, B.cons_id
                    | _  -> failwith "expected exactly one child, our succ"
                  end
                    
              | (`Succ_br _ | `Succ_ind) -> (* all children are Let_block-bound *)
                  (if children = [] 
                   then sl, B.cons_id 
                   else cons_let_children children)
                    
              | `Succ_cut(succ, _) -> (* don't Let_block-bind the cut's body *)
                  cons_let_children (children_without succ)
          in
          
          let succ_tree succ = 
            List.find (fun (DT.Tree(s, _)) -> s.sid = succ.sid) children in            
          
          match succ_kind with
            | `Succ_seq succ -> 
                (fun z -> 
                   begin cons_stmt
                     **> cons_tree sl sub cut_ret_site (succ_tree succ) 
                     **> z end)
                  
            | `Succ_cut (succ, ret) ->
                (fun z -> 
                   begin cons_let_children 
                     **> cons_stmt 
                     **> cons_tree sl sub (Some ret) (succ_tree succ) 
                     **> z end)
                  
            | `Succ_br succ -> begin match cut_ret_site with 
                | Some s when s.sid = succ.sid ->
                    let ret_vals = (List.map (pure_of_var sub) (phi_vars sub succ)) in
                    (fun z -> cons_stmt 
                       **> B.return (ppt_root (stmt_loc stmt) (Some fname)) ret_vals
                       **> z)
                      
                | _ ->  
                    let sl, nodes, br = mk_br sl sub (stmt_loc stmt) succ in
                    (fun z -> 
                       begin cons_stmt 
                         **> nodes
                         **> B.branch (ppt_root (stmt_loc stmt) (Some fname)) br
                         **> z end)
              end
                
            | `Succ_ind -> 
                (fun z -> 
                   begin cons_let_children
                     **> cons_stmt
                     **> z end)      
        end
      in
      match dom_tree with 
        | Some(dom_tree) -> cons_tree Stmt.Map.empty Subst.empty None dom_tree
        | None -> ignore (log "no stmts.@!") ; B.cons_id
            
  end (* Zipcfg.From_cil module *)

    
  (* Walk module : Defines a general-purpose walk of the graph.  As a
     special case, a walk's behavior can build a transformed graph
     from the one being walked over.

     The "walk behavior" is defined by a set of functions [w], of type
     [('a, 'b) walk_fns], which are parameterized by the two types
     ['a] and ['b].  The first type is that of the information moving
     "down" recursively through the graph.  The second type is that of
     the resulting information being "returned" back up the graph.
     Good mnuemonics for 'a and 'b are "from above" and "from below",
     respectively.

     For example, in the case of a graph transformation, 'a will be
     instansiated to any environment-like information flowing "down"
     (from above), and 'b will be instansiated as [G.zt], which
     effectively makes the walk build a new graph on the way back "up"
     (from below).
     
     For first and middle nodes, the walk functions ([w.walk_first] &
     [w.walk_middle]) take and return a ['a]; in addition, they return
     a function that, when given results from "below" (of type ['b]),
     will yield a result (also of type ['b]) for the node in question.
     For last nodes, there are no nodes "below", so the user-specified
     [w.walk_last] immediately returns its result (of type ['b]).

     Nested block definitions are treated somewhat as a special case.
     They are "bound" by a middle node (a Let_block node) which is
     walked first (using [w.walk_middle], as usual).  Then, we use
     [w.walk_nested] to obtain a new value moving down (of type ['a]),
     as well as a function to aggregate the results from below,
     including the results from processing each nested block
     definition.
  *)

  module Walk : sig
    (* 'a is the type of data "from above"
       'b is the type of data "from below"
    *)
    type ('a, 'b) walk_comb = 'a -> 'a * ('b -> 'b)

    val (@@>) : ('a, 'b) walk_comb -> ('a, 'b) walk_comb -> ('a, 'b) walk_comb
    val (@!>) : ('a, 'b) walk_comb -> ('a -> 'b)         -> ('a -> 'b)

    val id    : ('a, 'b) walk_comb
    val fold  : ('c -> ('a, 'b) walk_comb) -> 'c list -> ('a, 'b) walk_comb

    type ('a, 'b) walk_fns = {
      walk_first  : first      -> 'a -> 'a * ('b -> 'b) ;
      walk_middle : middle     -> 'a -> 'a * ('b -> 'b) ;
      walk_nested : label list -> 'a -> 'a * ('b list -> 'b -> 'b) ; 
      walk_last   : last       -> 'a -> 'b ;
    }

    val walk  : ('a, 'b) walk_fns -> 'a -> zt -> 'b

  end 
    = 
  struct
    type ('a, 'b) walk_fns = {
      walk_first  : first      -> 'a -> 'a * ('b -> 'b) ;
      walk_middle : middle     -> 'a -> 'a * ('b -> 'b) ;
      walk_nested : label list -> 'a -> 'a * ('b list -> 'b -> 'b) ; 
      walk_last   : last       -> 'a -> 'b ;
    }

    let ( **> ) f x = f x
    
    let (@@>) comb1 comb2 = fun a0 -> 
      let (a1, f) = comb1 a0 in
      let (a2, g) = comb2 a1 in
      (a2, fun b -> f **> g **> b)

    let (@!>) comb1 comb2 = fun a0 ->
      let (a1, f) = comb1 a0 in
      f **> comb2 a1

    let id = fun a -> (a, fun b -> b)

    let fold visit items =
      List.fold_right begin fun item comb ->
        visit item @@> comb 
      end items (fun a -> a, fun b -> b)        

    let walk (w:('a,'b) walk_fns) = 
      let rec walk_zblock (a:'a) ((head,tail),blocks) : 'b =
        let a, g = match head with
          | First first -> w.walk_first first a 
          | Head  _     -> a, (fun b -> b)
        in 
        g **> walk_tail a (fun b -> b) ((head, tail), blocks)
          
      and walk_tail (a:'a) (f:'b -> 'b)
          (((head,tail),blocks) as zgraph) : 'b =
        match tail with
          | Last last -> f (w.walk_last last a)
          | Tail (middle, tail) ->
              let a, g = w.walk_middle middle a in
              let    g = match middle with
                | Let_block (labels, _) ->
                    let a, (h : 'b list -> 'b -> 'b) = w.walk_nested labels a in
                    let do_block label = walk_zblock a (focus label (unfocus zgraph)) in
                    (fun b -> g **> h (List.map do_block labels) **> b)
                | _ -> g 
              in
              walk_tail a (fun b -> f **> g **> b) ((Head(head, middle), tail), blocks)
      in
      walk_zblock

    type ('a, 'b) walk_comb = 'a -> 'a * ('b -> 'b)
      


  end (* Walk module *)          
    
  (* Copy module: defines a walk behavior that copies the graph.  This
     behavior can be extended to do graph-to-graph transformations in an
     applicative (non-mutative) manner. *)
  module Copy : sig     
    val walk_fns      : ('a, zt) Walk.walk_fns
    val insert_nested : zt list -> zt -> zt
    val copyz         : zt -> zt
    val copy          : t  -> t
  end 
    = 
  struct
    module B = Build
    module W = Walk

    let insert_nested (zs:zt list) (z:zt) : zt = 
      List.fold_left begin fun (zblock, blocks) z ->
        (zblock, Blocks.union blocks (unfocus z))
      end z zs

    let walk_fns = { W.walk_first  = begin fun f a -> (a, B.cons_first f)    end ;
                     W.walk_middle = begin fun m a -> (a, B.cons_middle m)   end ;
                     W.walk_nested = begin fun _ a -> (a, insert_nested)     end ;
                     W.walk_last   = begin fun l a -> (B.cons_last l emptyz) end ;
                   }
    let copyz (z:zt) : zt = 
      W.walk walk_fns () z

    let copy (graph:t) : t =
      unfocus (W.walk walk_fns () (entry graph))
  end (* Copy module *)

        
  (* Compute the free variables of a graph *)
  module Free_vars : sig
    type varset = Varinfo.Set.t
        
    val fv_of_lval   : lval      -> varset
    val fv_of_pure   : pure      -> varset
    val fv_of_pures  : pure list -> varset
    val fv_of_simp   : simp      -> varset
    val fv_of_memo   : memo      -> varset
    val fv_of_scope  : scope     -> varset
    val fv_of_middle : middle    -> varset
    val fv_of_last   : last      -> varset
    val fv_of_graph  : t         -> varset

  end = struct    
    module W = Walk
    module V = Varinfo.Set
    type varset = V.t

    let fv_of_lval lval = Free_vars.of_lval lval   
    let fv_of_pure pure = Free_vars.of_exp pure   

    let ( ++ ) a b = V.union a b
    let ( -- ) a b = V.diff  a b
      
    let fv_of_vars vars = Varinfo.set_from_list vars

    let fv_of_pures pures = 
      List.fold_left 
        (fun vars pure -> fv_of_pure pure ++ vars) 
        V.empty pures
                  
    let fv_of_simp = function
      | Pure pure         -> fv_of_pure pure
      | Read lval         -> fv_of_lval lval
      | Low (_, pures)    -> fv_of_pures pures
      | Ffi (_, _, pures) -> fv_of_pures pures
      | Clib (pure, pures)-> fv_of_pure pure ++ fv_of_pures pures
      | Peek lval         -> fv_of_lval lval
      | Poke (lval, pure) -> fv_of_lval lval ++ fv_of_pure pure
      | Scope             -> V.empty
      | Alloc _           -> V.empty (* TEMP: assume no dependent types 
                                        -- TODO: handle arrays? *)
    let fv_of_scope = function
      | Scope_same        -> V.empty
      | Scope_change pure -> fv_of_pure pure

    let fv_of_memo = function
      | Memo_no             -> V.empty
      | Memo_yes (_, scope) -> fv_of_scope scope

    let fv_of_middle = function
      | Let_block _           -> V.empty
      | Cut (_, memo,_)       -> fv_of_memo memo
      | Memo (scope, _)       -> fv_of_scope scope
      | Update _              -> V.empty
      | Let_simp (_, simp, _) -> fv_of_simp simp
      | Write (lval, pure, _) -> fv_of_lval lval ++ fv_of_pure pure
      | Rvars (vars, _)       -> fv_of_vars vars

    let fv_of_branch = function
      | Br (_, pures) -> fv_of_pures pures

    let fv_of_last = function
      | Exit                     -> V.empty
      | Cond (pure, br1, br2, _) -> fv_of_pure pure ++ fv_of_branch br1 ++ fv_of_branch br2
      | Branch (br, _)           -> fv_of_branch br
      | Call (_, pure, pures, _) -> fv_of_pure pure ++ fv_of_pures pures
      | Return (pures, _)        -> fv_of_pures pures

    (* TODO -- should we check for closed code; or just compute free
       variables?  If we do the former, then we need to thread down a
       set of variables allowed to appear free, and then thread back
       up a subset of this first set: those variables that actually do
       appear free. 
       
       If we do the latter (only compute free variables, ignoring
       whether or not these free variables are well-scoped) then we need
       not thread through the first set of variables.
    *)
    let walk_fns : (varset, varset) W.walk_fns = {
      W.walk_first = begin fun first fv ->
        raise NYI
      end ;
      
      W.walk_nested = begin fun labels fv ->
        raise NYI
      end ;
      
      W.walk_middle = begin fun middle fv ->
        raise NYI
      end ;
      
      W.walk_last = begin fun last fv ->
        raise NYI
      end ;      
    }

    let fv_of_graph = fun _ -> raise NYI
  end (* Free_vars module *)


  (* TODO -- Refactor this code to use Walk module *)
  module Print = struct
    open Rep
    module P = Pretty

    let print_program_points = ref true

    let rec string_of_labels = function 
      | l::[] -> (string_of_label l)
      | l::ls -> (string_of_label l)^" and "^(string_of_labels ls)
      | []    -> ""
          
    let pr_list f sep xs = 
      let rec loop = function
        | [] -> ""
        | x::[] -> f x
        | x::xs -> (f x)^sep^(loop xs)
      in loop xs
                 
    let pr_br = function
      | Br(label, pures) -> 
          (string_of_label label)
          ^"("^(pr_list string_of_exp ", " pures)^")"

    let (++) = P.(++)
    let indent = 4

    let string_of_property = function
      | Property.Undo_code  -> "undo_code"
      | Property.Memo_code  -> "memo_code"
      | Property.Root_entry -> "root_entry"
          
    let pr_props ppt = 
      match (Ppt.props ppt) with 
        | [] -> ""
        | _  -> " {"^(pr_list string_of_property "," (Ppt.props ppt)) ^"}"

    let pr_ppt _ ppt = 
      if !print_program_points
      then P.dprintf "%s%s@!" (Ppt.string ppt)  (pr_props ppt)
      else P.nil

    let d_typseq = P.docList ~sep:(P.text ", ") (Cil.d_type ())

    let pr_pures_doc = 
      P.docList ~sep:(P.text ", ") (fun exp -> P.text (string_of_exp exp))

    let pr_vars_doc =
      P.docList ~sep:(P.text ", ") (fun v -> P.dprintf "%s : %s" v.vname (string_of_type v.vtype))
        
    let string_of_fnexp = function
      | Default  -> string_of_exp
      | Run_core -> (fun e -> "core("^(string_of_exp e)^")")

    let pr_last_doc = function
      | Exit -> P.dprintf "((exit))"
          
      | Cond(pure, br1, br2, pp) ->  
          (P.dprintf "%aif %s@?then %s@?else %s" pr_ppt pp
             (string_of_exp pure) (pr_br br1) (pr_br br2)) 
            
      | Branch (br, pp) -> P.dprintf "%a%s" pr_ppt pp (pr_br br) 
          
      | Call (mode, pure, pures, pp) -> P.dprintf "%a%s (%a)" pr_ppt pp
          (string_of_fnexp mode pure) pr_pures_doc pures 

      | Return (pures, pp) -> P.dprintf "%areturn (%a)" pr_ppt pp
          pr_pures_doc pures 
            
    let pr_simp_doc () = function        
      | Alloc typ -> P.dprintf "alloc (%s)" (string_of_type typ)
      | Read lval -> P.dprintf "read (%s)"  (string_of_lval lval)
      | Pure pure -> P.dprintf "pure (%s)"  (string_of_exp pure)
      | Peek lval -> P.dprintf "peek (%s)"  (string_of_lval lval)
      | Scope     -> P.dprintf "scope ()"
      
      | Poke (lval, pure) -> P.dprintf "poke (%s, %s)" 
          (string_of_lval lval) (string_of_exp pure)
      
      | Low  (low, pures) -> P.dprintf "%s (%a)" 
          (Low.name_of_op low) pr_pures_doc pures
      
      | Clib (pure, pures) -> P.dprintf "%s (%a)"
          (string_of_exp pure) pr_pures_doc pures

      | Ffi  (Ffi.Srcvar, op, pures) -> P.dprintf "%s (%a)" 
          op.Ffi.srcvar.Cil.vname pr_pures_doc pures
      
      | Ffi  (view, op, pures) -> P.dprintf "%s[%s] (%a)"
          op.Ffi.srcvar.Cil.vname (Ffi.string_of_view view)
            pr_pures_doc pures
            
    let pr_scope () = function
      | Scope_change pure -> P.dprintf"(%s)" (string_of_exp pure)
      | Scope_same        -> P.nil

    let pr_memo () = function
      | Memo_no              -> P.nil
      | Memo_yes (pp, scope) -> P.dprintf "%amemo@?%a" pr_ppt pp pr_scope scope

    let rec pr_tail_doc ((h,t), blocks) : P.doc = match t with
      | Last last -> 
          pr_last_doc last
            
      | Tail(middle, tail) ->
          let pr_middle_doc = function
            | Let_simp (var, simp, pp) -> 
                P.dprintf "%alet %s : %s = %a in@!" pr_ppt pp
                  var.vname (string_of_type var.vtype) pr_simp_doc simp 
                  
            | Write (lval, pure, pp) -> 
                P.dprintf "%a%s := %s;@!" pr_ppt pp
                  (string_of_lval lval) (string_of_exp pure) 

            | Cut (label, memo, pp) -> 
                P.dprintf "%a%s(cut %a@!" pr_ppt pp
                  (string_of_label label) pr_memo memo                  
                
            | Memo (s, pp) -> 
                P.dprintf "%amemo@?%a@!" pr_ppt pp pr_scope s
            
            | Update pp -> 
                P.dprintf "%aupdate@!" pr_ppt pp 

            | Rvars(vars, pp) ->
                P.dprintf "%arvars (%a)@!" pr_ppt pp pr_vars_doc vars

            | Let_block (labels, pp)   -> 
                let bs : (zblock * Blocks.t) list
                    = List.map (fun lab -> (unzip (Blocks.find lab blocks), blocks)) labels in
                P.dprintf "%aletblk@!%a@!in@!" pr_ppt pp
                  (P.docList ~sep:(P.dprintf "@!andblk@!") pr_block_doc) bs
          in
          let middle_doc = pr_middle_doc middle in
          let tail_doc   = pr_tail_doc ((Head(h, middle), tail), blocks) in
          match middle with
            | Cut _ -> middle_doc ++ (P.indent indent (tail_doc ++ (P.text " )")))
            | _     -> middle_doc ++ tail_doc

    and pr_block_doc ((h,t),blocks) : P.doc =
      match h with Head _ -> assert false | First f ->
        let pr_first = function 
          | Entry -> if false then (P.text "((entry))") ++ P.break else P.text ""
              
          | Label(label, _, vars, pp) -> P.dprintf "%a%s (%a) = @!" pr_ppt pp
              (string_of_label label) pr_vars_doc vars
        in      
        (pr_first f) ++ (P.indent indent (pr_tail_doc ((First f, t), blocks)))
                  
    let pr_graph_doc (graph:t) : P.doc = pr_block_doc (entry graph)

    let pr_fundec_doc (fundec:fundec) (graph:t) = 
      let (_, _, return_types) = Annot.arrow_type_unpack fundec.svar.vtype in
      (P.dprintf "function %s (%a) : %a = @!" 
         fundec.svar.vname 
         pr_vars_doc fundec.sformals
         d_typseq return_types)
       ++ (pr_graph_doc graph) ++ (P.dprintf "@!@!")
        
  end (* Zipcfg.Pretty module *)
    
end (* Zipcfg module *) 


(* For each label, assign a boolean saying whether or not that basic
   block is always semantically equivalent to a fixed return sequence
   of pure expressions (and no other computation of any kind).

   That is, if the analysis says "Yes, basic block L is equivalent to
   sequence [pure_1; ...; pure_k]" then anytime one sees "L(pure'_1,
   ..., pure'_m)" one can safely substitute the branch with a return
   of pures, each of the form [pure'_i/x_i] pure_j where 1<=i<=k and
   1<=j<=m.
*)
module Return_equiv : sig 
  val of_graph : Zipcfg.zt -> (Zipcfg.label -> Zipcfg.pure list option)
end 
  = 
struct
  module G = Zipcfg
  module W = G.Walk
  module M = Label.Map
  type rseq = G.pure list
      
  type elt = Maybe | Yes of rseq | No
      
  let string_of_elt = function
    | Maybe -> "Maybe"
    | Yes _ -> "Yes _"
    | No    -> "No"

  let string_of_mapping label elt =
    Printf.sprintf "%s --> %s"
      (string_of_label label)
      (string_of_elt elt)
      
  (* meet two lattice elements. *)
  let meet yes a b : elt = begin 
    match (a,b) with
      | _,      No     -> No
      | No,     _      -> No
      | Yes xs, Maybe  -> Yes xs
      | Maybe,  Yes ys -> Yes ys
      | Maybe,  Maybe  -> Maybe
      | Yes xs, Yes ys -> yes xs ys
  end (* match *)

  let elt_eq a b = begin 
    match a, b with
      | No,      No     -> true
      | Maybe,   Maybe  -> true
      | Yes xs , Yes ys -> List.for_all2 (==) xs ys
      | _,      _       -> false
  end (* match *)
    
  type map = elt M.t
  type above = unit
  type below = { elt : elt ; map : map }        
  
  let find label map = 
    if M.mem label map 
    then M.find label map
    else Maybe

  (* - - - utility combinators - - - - *)

  let congruent    = fun a -> a, fun b -> b
  let save   label = fun a -> a, fun b -> { b with map = M.add label b.elt b.map }
  let set_elt  elt = fun a -> a, fun b -> { b with elt = elt }
  let set_elt' elt = fun a -> { map = M.empty ; elt = elt }

  let cut label =       
    fun a -> a, fun b -> 
      { b with elt = begin meet 
            begin fun xs ys -> 
              if (xs, ys) = ([], []) then Yes []
              else raise NYI (* [xs / vars] ys *)
            end b.elt (find label b.map)
        end }
        
  (* - - walk each node - - *)

  let walk_fns map_0 = { 
    W.walk_first = begin function 
      | G.Entry                -> congruent
      | G.Label (label, _,_,_) -> save label
    end ;
    W.walk_middle = begin function
      | G.Let_simp  _       -> set_elt No
      | G.Write     _       -> set_elt No
      | G.Cut (label, _, _) -> cut label
      | G.Memo      _       -> congruent
      | G.Update    _       -> congruent
      | G.Rvars     _       -> congruent
      | G.Let_block _       -> congruent
    end ;
    W.walk_last = begin 
      let br = function 
        | G.Br (label, _) -> find label map_0
      in 
      let cond e1 e2 = set_elt'
        (meet (fun xs ys -> if xs = ys 
               then Yes xs else No) e1 e2)
      in
      function
        | G.Exit                  -> set_elt' Maybe
        | G.Cond   (_, b1, b2, _) -> cond (br b1) (br b2)
        | G.Branch (b, _)         -> set_elt' (br b)
        | G.Call   (_,_,_,_)      -> set_elt' No
        | G.Return (rseq, _)      -> set_elt' (Yes rseq)
    end ;
    W.walk_nested = begin fun _ a -> a, 
      (* TODO -- see notes about making this interface more uniform;
         design the postpone combinator to switch to this style *)
      fun bs b -> 
        { b with map = List.fold_right begin fun map1 map2 ->
            M.fold begin fun label elt map ->
              let elt' = meet begin fun xs ys -> 
                if xs = ys then Yes xs else No end 
                (find label map) elt
              in
              (* ignore (log "merged : %s\n" (string_of_mapping label elt')) ; *)
              M.add label elt' map
            end map1 map2            
          end (List.map (fun b -> b.map) bs) b.map
        }
    end
  }
    
  let of_graph graph = 
    let map : map =
      let rec loop_until_fixpoint map_0 =
        (* begin (* DEBUG / TEMP: *)
          ignore (log "return-equiv map: \n") ;
          M.iter begin fun label elt ->
            ignore (log " : %s\n" (string_of_mapping label elt)) ;
          end map_0 
        end 
        ; *)
        let map = (W.walk (walk_fns map_0) () graph).map in
        (*let _ = Printf.fprintf stderr "Return_equiv: checking for a fixpoint...\n%!" in*)
        if M.equal elt_eq map map_0 then 
          (*let _ = Printf.fprintf stderr "Return_equiv: found a fixpoint.\n%!" in*)
          map_0
        else begin 
          (*let _ = Printf.fprintf stderr "Return_equiv: no fixpoint yet.  Doing another pass...\n%!" in*)
          loop_until_fixpoint map
        end
      in 
      (*let _ = Printf.fprintf stderr "Return_equiv: doing a first pass...\n%!" in*)
      loop_until_fixpoint M.empty
    in
    (* ignore (log "return-equiv reached fixpoint:\n") ;
    M.iter begin fun label elt ->
      ignore (log " : %s\n" (string_of_mapping label elt)) ;
    end map ;
    ignore (log "\n") ; *)
    (fun label -> begin try match M.find label map with
       | No     -> None
       | Yes xs -> Some xs
       | Maybe  -> assert false
     with Not_found -> 
       ignore (log "couldn't find %s\n\n" (string_of_label label)) ;
       assert false
     end)
end


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - *)
(* Lift_returns uses the analysis above (in Return_equiv module) to
   simplify a graph with respect to returns; the effect of this
   transformation is to duplicate across branch-indirection.
   
   That is, we:
   
   -- duplicate a return at the source of a branch when the branch
   targets a block that is equivalent to a return. 
   
   -- do something about cuts ....
*)
module Lift_returns = struct
  module G  = Zipcfg
  module W  = G.Walk
  module B  = G.Build
  module Ppt = Program_point

  module Subst : sig
    type t
    val of_branch : G.t -> G.branch -> t
    val app       : t -> G.pure -> G.pure
  end 
    = 
  struct
    module M = Varinfo.Map
    type t = G.pure M.t

    class visitor sub = object
      inherit Cil.nopCilVisitor
      method vexpr (exp:Cil.exp) = match exp with
        | Cil.Lval (Cil.Var v, Cil.NoOffset) -> 
            if M.mem v sub 
            then Cil.ChangeTo (M.find v sub)
            else Cil.DoChildren

        | Cil.Lval (Cil.Var v, offset) -> 
            (* TODO: do we need to handle this case? *)
            assert false

        | _ -> Cil.DoChildren
    end

    let app sub pure =
      Cil.visitCilExpr (new visitor sub) pure

    let of_branch graph : G.branch -> t = function
      | G.Br (label, pures) ->
          let (_,_,vars,_) = G.info_of_label label graph in
          List.fold_right2 M.add vars pures M.empty

  end (* Subst module *)

  (* Lift up returns in the given graph using a return-equivalence
     function defined over the basic blocks of the graph -- see
     Return_equiv module for a definition of this. *)
  let of_graph (graphz:G.zt) : G.zt =     
    let return_eq = Return_equiv.of_graph graphz in      
    let graph     = G.unfocus graphz in

    (* Transforms a branch into either a return (if the branch is
       equivalent to a return) or else leaves it alone *)
    let of_br ppt : G.branch -> G.nodes = function
      | (G.Br (label, _)) as br -> match return_eq label with
          | None                 -> B.branch ppt br 
          | Some pures_to_return -> B.return ppt begin
              List.map 
                (Subst.app (Subst.of_branch graph br)) 
                pures_to_return
            end
    in      
    W.walk 
      { G.Copy.walk_fns with 
          W.walk_middle = begin function
            | (G.Cut (label, memo, ppt)) as middle -> fun a -> a,
                begin match return_eq label, G.info_of_label label graph, memo with
                  | Some [], (_,_,[],_), G.Memo_no    -> 
                      B.cons_id

                  | Some [], (_,_,[],_), G.Memo_yes (ppt, s) -> 
                      B.memo ppt s

                  | Some pures, (_,_,vars,_), memo ->
                      let vars_all_match : bool = 
                        try 
                          List.fold_left2 begin fun flag pure var ->
                            match pure with
                              | Cil.Lval (Cil.Var v, Cil.NoOffset) when v == var -> flag
                              | _ -> false
                          end true pures vars
                        with 
                          | Invalid_argument "List.fold_left2" -> false
                      in 
                      begin match vars_all_match, memo with
                        | true,  G.Memo_yes (pt, s) -> B.memo ppt s
                        | true,  G.Memo_no          -> B.cons_id
                        | false, _                  -> B.cons_middle middle
                      end

                  | _ -> B.cons_middle middle
                end
            | middle -> G.Copy.walk_fns.W.walk_middle middle 
          end ;

          W.walk_last = begin function
            | G.Branch (br, ppt) -> (fun _ -> of_br ppt br G.emptyz)
            | last              -> G.Copy.walk_fns.W.walk_last last
          end ;
      } () graphz
end
  


(* Our representation of CEAL programs.  This representation is
   analogous to the Cil.file datatype, though it is purely-functional and
   endowed with a zipper-like interface. *)
module Program = struct
  module G = Zipcfg
    
  type   func  = G.var * G.var list * G.t * G.ppt
  module Funcs = Varinfo.Map
  type   funcs = func Funcs.t
  type   mark  = int
  
  (* The level descriptor says whether the code following a mark is in
     the meta level, the core level, or both.  In the case of both, the
     code following this mark must be identical in both levels (up to the
     next mark). *)
  type level = Meta | Core | Both
      
  (* Globals *)
  type glob = 
    | Vard of G.var          * G.ppt (* subsumes Cil.GVarDecl *)
    | Var  of G.var * G.simp * G.ppt (* subsumes Cil.GVar     *)
    | Func of G.var          * G.ppt (* subsumes Cil.GFun     *)
    | Cilg of Cil.global     * G.ppt
    | Mark of mark * level (* for separating/merging program levels *)
        
  (* Global zipper *)
  type head = First | Head of head * glob
  type tail = Last  | Tail of glob * tail      

  (* Program fragments: a global zipper, a set of funcs and a
     combinator for initization code.  *)
  and  frag = (head * tail) * funcs * G.nodes  
      
  (* A combinator transforms fragments (e.g., by "inserting" stuff in them) *)
  type comb = frag -> frag
        
  (* A program is a "closed" fragment *)
  type t = glob list * funcs * G.t

  (* An empty fragment *)
  let empty_frag : frag = 
    ((First, Last), Funcs.empty, G.Build.cons_id)

  (* Zip up a zipper, yeilding just a tail *)
  let rec zip : head * tail -> tail = function
    | (First,          t) -> t
    | (Head (h, glob), t) -> zip (h, Tail(glob, t))

  (* Convert a tail into a list of globals *)
  let list_of_tail : tail -> glob list = 
    let rec loop globs = function 
      | Last           -> List.rev globs
      | Tail (glob, t) -> loop (glob::globs) t
    in loop [] 

  (* Convert a list of globals into a head *)
  let head_of_list : glob list -> head =
    let rec loop head = function
      | []          -> head
      | glob::globs -> loop (Head(head, glob)) globs
    in loop First

  (* The program corresponding to the given fragment  *)
  let prog_of_frag : frag -> t = 
    fun (ht, funcs, nodes) -> 
      (list_of_tail (zip ht), funcs, 
       G.unfocus (nodes G.emptyz))

  (* A fragment corresponding to the given program *)
  let frag_of_prog : t -> frag = 
    fun (globs, funcs, graph) -> 
      ((First, zip (head_of_list globs, Last)),
       funcs, G.nodes_of_graph graph)
      
  (* Create a combinator from a fragment *)
  let comb_of_frag : frag -> comb = 
    fun frag (globs, funcs, nodes) ->
      assert (globs = (First, Last)) ;
      assert (funcs = Funcs.empty) ;
      assert (nodes G.emptyz = G.emptyz) ;
      frag

  exception Duplicate_func of G.var

  (* Merge two Funcs into a single one *)
  let merge_funcs funcs1 funcs2 =
    Funcs.fold begin fun f func funcs ->
      if Funcs.mem f funcs 
      then raise (Duplicate_func f)
      else Funcs.add f func funcs
    end funcs1 funcs2

  let func_var_is_foreign_c program var =
    let _,funcs,_ = program in
    let is_foreign_c = not (Funcs.mem var funcs) in
    let _ = if false then (* TEMP Debugging. *)
      ( if not is_foreign_c 
        then ignore (log "TEMP 1: core/meta : %s\n" var.Cil.vname)
        else ignore (log "TEMP 2: foreign_c  : %s\n" var.Cil.vname)
      )
    else ()
    in is_foreign_c

  (* Supplies combinators to build programs *)
  module Build : sig    
    val empty : comb
    
    val glob  : glob -> comb

    val vard  : G.ppt -> G.var           -> comb
    val var   : G.ppt -> G.var -> G.simp -> comb
    val cil   : G.ppt -> Cil.global      -> comb

    val mark  : level -> comb
    val mark' : mark -> level -> comb
    
    (* Functions consist of a global variable (the function name) as
       well as a definition (including the functions parameters and
       its body of code); We provide combinators for building either
       the global variable only ([func']), the definition only
       ([func'']) or both ([func]). 
    *)
    val func  : G.ppt -> func  -> comb (* func global & body *)
    val func' : G.ppt -> G.var -> comb (* func global only. *)
    val func'': G.ppt -> func  -> comb (* func body only. *)
  end 
    = 
  struct
    let empty = fun (frag:frag) -> frag

    let glob (glob:glob) = 
      fun ((h, t), funcs, nodes) ->
        ((h, Tail(glob, t)), funcs, nodes)

    let vard ppt (var:G.var) =
      glob (Vard (var, G.Ppt.from ppt))

    let var ppt (var:G.var) (simp:G.simp) =
      glob (Var (var, simp, G.Ppt.from ppt))

    let func' ppt var =
      fun (z,funcs,nodes) ->
        glob (Func (var, G.Ppt.from ppt))
        **> (z, funcs, nodes)

    let func'' ppt ((var,_,_,_) as func:func) =
      fun (z,funcs,nodes) ->
        if Funcs.mem var funcs 
        then raise (Duplicate_func var)
        else (z, Funcs.add var func funcs, nodes)

    let func ppt ((var,_,_,_) as func:func) = 
      (fun frag -> 
         begin func'  ppt var 
           **> func'' ppt func 
           **> frag end)
      (*
      fun (z,funcs,nodes) ->
        glob (Func (var, Program_point.gen loc))
        **> (z, Funcs.add var func funcs, nodes)
      *)

          
    let cil ppt (cil:Cil.global) = match cil with
      | ( Cil.GVarDecl _ 
(*        | Cil.GVar     _ *)
        | Cil.GFun     _ ) -> assert false      
      | Cil.GAsm _ -> raise NYI
      | cil -> 
          glob (Cilg (cil, G.Ppt.from ppt))

    let mark' (mark:mark) (level:level) =
      glob (Mark (mark, level))
        
    let fresh_mark = 
      let next_mark = ref 0 in
      fun _ -> 
        let m = !next_mark in
        let _ = incr next_mark in m

    let mark (level:level) =
      mark' (fresh_mark()) level

  end

  module Walk : sig
    type ('a, 'b) walk_fns = {
      walk_glob : glob -> 'a -> 'a * ('b -> 'b) ;
      walk_func : func -> 'a -> 'a * ('b -> 'b) ;
      walk_root : G.t  -> 'a -> 'b ;
    }        
        
    val walk : ('a, 'b) walk_fns -> 'a -> t -> 'b 
  end 
    = 
  struct
    type ('a, 'b) walk_fns = {
      walk_glob : glob -> 'a -> 'a * ('b -> 'b) ;
      walk_func : func -> 'a -> 'a * ('b -> 'b) ;
      walk_root : G.t  -> 'a -> 'b ;
    }        

    let walk (w:('a, 'b) walk_fns) (a:'a) 
        ((globs, funcs, init) : t) : 'b =
      let rec walk_globs (a:'a) (f:'b -> 'b) = function
        | []              -> f **> w.walk_root init a
        | glob :: globs -> 
            let a, g = w.walk_glob glob a in
            let    g = match glob with
              | Func (var, _) -> 
                  let a, h = w.walk_func (Funcs.find var funcs) a in
                  (fun b -> g **> h **> b)
              | _ -> g
            in
            walk_globs a (fun b -> f **> g **> b) globs
      in
      walk_globs a (fun b -> b) globs

  end (* Walk module *)

  module Copy : sig
    val walk_fns : ('a, comb) Walk.walk_fns
    val copy     : t -> comb
  end 
    = 
  struct
    module B = Build
    module W = Walk

    let noop = fun z -> z

    let walk_fns = {
      W.walk_glob = begin fun g a -> a, 
        (fun comb z -> B.glob g **> comb **> z)
      end ;

      W.walk_func = begin fun ((_,_,_,ppt) as f) a -> a, 
        (fun comb z -> B.func'' (G.Ppt.from ppt) f **> comb **> z)
      end ;

      W.walk_root = begin fun r a -> noop end ;
    }

    let copy (program:t) : comb =
      W.walk walk_fns () program
  end

  module Print = struct
    module P = Pretty
    module W = Walk
      
    let (++) = P.(++)

    let pr_func (func:func) : P.doc -> P.doc = fun doc ->
      let (fvar, fargs, fgraph, fpp) = func in
      let (_, _, rtypes) = Annot.arrow_type_unpack fvar.Cil.vtype in
      (P.dprintf "%afunc %s (%a) : (%a) = @!" 
         G.Print.pr_ppt fpp fvar.Cil.vname G.Print.pr_vars_doc fargs
         G.Print.d_typseq rtypes)
      ++ (G.Print.pr_graph_doc fgraph) 
      ++ (P.dprintf "@!@!")
      (* ++ (P.dprintf "in@!@!") *)
      ++ doc

    let walk_fns : (unit, P.doc) W.walk_fns = {      
      W.walk_glob = begin fun glob a -> 
        let glob_doc = match glob with
          | Func (var, pt) -> (fun doc -> doc) (* TODO *)
          
          | Vard  (var, pt) -> 
              (fun doc -> P.dprintf "%avar %s : %a;@!@!" 
                 Zipcfg.Print.pr_ppt pt
                 var.Cil.vname Cil.d_type var.Cil.vtype ++ doc)
          
          | Var  (var, simp, pt) -> 
              (fun doc -> P.dprintf "%alet %s : %a = %a;@!@!" 
                 Zipcfg.Print.pr_ppt pt
                 var.Cil.vname Cil.d_type var.Cil.vtype 
                 Zipcfg.Print.pr_simp_doc simp ++ doc)

          | Cilg (glb, pt) -> 
              (fun doc -> P.dprintf "%a%a@!" 
                 Zipcfg.Print.pr_ppt pt Cil.d_global glb ++ doc) 
          
          | Mark (mark, level) -> 
              (fun doc -> P.dprintf "/* mark:%d, level:%s */@!" mark
                 (match level with
                    | Core -> "core"
                    | Meta -> "meta"
                    | Both -> "meta+core") ++ doc)
        in
        (a, glob_doc) end ;

      W.walk_func = begin fun func a -> (a, pr_func func)           end ;
      W.walk_root = begin fun graph _ -> G.Print.pr_graph_doc graph end ;
    }

    let pr_prog (prog:t) : P.doc =
      W.walk walk_fns () prog       

  end (* Print module *)

  module From_cil = struct
    module B = Build

    let gen_comb ffi_env = function
      | ( Cil.GType        _
        | Cil.GCompTag     _
        | Cil.GCompTagDecl _
        | Cil.GEnumTag     _
        | Cil.GEnumTagDecl _
        | Cil.GText        _ ) as glob ->
          ffi_env, B.cil (G.Ppt.root (Cil.get_globalLoc glob) None) glob 
           
      | (Cil.GPragma (attr, loc)) as glob ->
          let ffi_env' = Ffi.From_cil.pragma attr ffi_env 
          in ffi_env, 
          ( if Ffi.From_cil.equal ffi_env ffi_env' 
            then (* not our pragma, keep it there: *) B.cil (G.Ppt.root loc None) glob
            else (* our pragma; remove it: *) B.empty )
            
      | Cil.GAsm _ -> raise NYI
          
      | Cil.GVarDecl (var, loc) ->
          (Ffi.From_cil.var var ffi_env),
          B.vard (G.Ppt.root loc None) var 
            
      | Cil.GVar (var, initinfo, loc) ->
          (* Only a couple of initializations are valid here. *)
          ffi_env, 
          begin match initinfo.Cil.init with
            | None                      -> B.vard (G.Ppt.root loc None) var
            | Some (Cil.CompoundInit _) -> assert false
            | Some (Cil.SingleInit exp) ->                           
                let simp = match Annot.kw_of_exp exp with
                  | None                   -> G.Pure exp
                  | Some (Annot.Alloc typ) -> G.Alloc typ
                  | _                      -> assert false
                in B.var (G.Ppt.root loc None) var simp
          end

      | Cil.GFun (fundec, loc) ->
          (*let _ = ignore (log "%s\n" fundec.Cil.svar.Cil.vname ) in*)
          (Ffi.From_cil.var fundec.Cil.svar ffi_env),
          let func_nodes = G.From_cil.fundec ffi_env fundec in
          B.func (G.Ppt.root loc None)
            (fundec.Cil.svar, 
             fundec.Cil.sformals,
             G.unfocus (func_nodes G.emptyz), 
             G.Ppt.root loc None)

    let file (file:file) : comb = 
      let rec loop ffi_env (prog:comb) = function
        | [] -> begin 
            match file.Cil.globinit with
              | None   -> prog
              | Some g -> 
                  let ffi_env, comb = 
                    gen_comb ffi_env (Cil.GFun (g, locUnknown)) 
                  in
                  fun z -> prog **> comb **> z
          end

        | glob :: globs -> 
            let ffi_env, comb = gen_comb ffi_env glob in
            loop ffi_env (fun z -> prog **> comb **> z) globs
      in 
      (* TODO: instead of empty, start with file.Cil.globinit nodes *)
      loop Ffi.From_cil.empty B.empty file.Cil.globals

  end (* From_cil module *)
    
end (* Program module *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Return_types : sig
  val of_func : Program.func -> (Zipcfg.ppt -> Cil.typ list)
end = struct
  module G = Zipcfg
  
  module W = Zipcfg.Walk
  let (@@>) = G.Walk.(@@>)

  module M = G.Ppt.Map

  type types = Cil.typ list
  type above = types
  type below = types M.t

  let save ppt = 
    fun return_types -> return_types, 
      fun map -> M.add ppt return_types map

  let nop = fun a -> a, fun b -> b

  let cut_region ppt return_types' =
    fun return_types -> return_types',
      fun map -> M.add ppt return_types map

  let of_func (var, vars, graph, pt) = 
    (* Get the return type(s) of the function using the type of its
       variable (its name) *)
    let (_, _, return_types) = 
      Annot.arrow_type_unpack var.Cil.vtype 
    in
    (* Construct a map that sends program points to return types *)
    let map = W.walk {
      
      W.walk_first = begin function
        | G.Entry -> (fun a -> a, fun b -> b)
        | G.Label (_,_,_,ppt) -> save ppt
      end (* walk_first *);

      W.walk_nested = begin fun _ a -> a, 
        List.fold_right (M.fold M.add)
      end (* walk_nested *);

      W.walk_middle = begin fun m -> match m with
        | G.Cut (label, memo, ppt) -> 
            begin cut_region ppt begin 
              List.map (fun v -> v.Cil.vtype)
                (G.formals_of_label label graph)                
            end
                (* Save the return value of the cut body as that of the memo point,
                   assuming there is a memo point. *)
              @@> match memo with
                | G.Memo_no          -> nop
                | G.Memo_yes (pt, _) -> save pt
            end

        | m -> save (G.ppt_of_middle m)
      end (* walk_middle *) ;

      W.walk_last = begin fun l -> 
        match G.ppt_of_last l with
          | None    -> (fun _ -> M.empty)
          | Some ppt -> begin fun return_types -> 
              M.add ppt return_types (M.empty) end
      end (* walk_last *) ;

      } return_types (G.entry graph)
    in
    (* map program points to return types using the map we
       constructed. *)
    begin fun ppt -> 
      M.find ppt map
    end
end (* Return_types *)


module Type_check : sig
  
  val of_program : Program.t -> bool

end = struct
  module G = Zipcfg
  module P = Program
  module Pr = Pretty
         
  type typ  = G.typ
  type typs = G.typ list
  
  module Vars = Varinfo.Set
  module Typs = Type.Set
  module Labs = Label.Map

  let ( **> ) f x = f x
  let ( @@> ) f g = G.Walk.( @@> ) f g
  let ( @!> ) f g = G.Walk.( @!> ) f g   

  (* Errors that may arise while we type-check the program: *)
  type err =
    | Type_mismatch   of typ  * typ
    | Types_mismatch  of typs * typs
    | Duplicate_var   of G.var
    | Duplicate_label of G.label
    | Unbound_var     of G.var
    | Unbound_label   of G.label
    | Not_a_scalar    of G.pure (* conditional pures are always scalars. *)
    | Not_a_func_var  of G.pure (* function calls are must be first-order. *)
    | Qual_var_type   of G.var * Qual.t (* variables should not be qualified (they are not storage). *)

  (* Print an error in a human-friendly-ish way. *)
  let d_err : err -> Pr.doc = function
    | Type_mismatch  (t1, t2)   -> Pr.dprintf "type mismatch: expected %a, got %a" Cil.d_type t1 Cil.d_type t2
    | Types_mismatch (ts1, ts2) -> Pr.dprintf "type sequence mismatch: expected (%a), got (%a)" G.Print.d_typseq ts1 G.Print.d_typseq ts2
    | Duplicate_var   var       -> Pr.dprintf "definition of variable defined earlier: %s" (string_of_var var)
    | Duplicate_label label     -> Pr.dprintf "definition of basic-block label defined earlier: %s" (string_of_label label)
    | Unbound_label   label     -> Pr.dprintf "use of undefined label: %s" (string_of_label label)
    | Unbound_var     var       -> Pr.dprintf "use of undefined variable: %s" (string_of_var var)
    | Not_a_scalar    pure      -> Pr.dprintf "expected a scalar, but instead got: %a : %a" Cil.d_exp pure Cil.d_type (Cil.typeOf pure)
    | Not_a_func_var  pure      -> Pr.dprintf "we expected a function name, but instead got: %a" Cil.d_exp pure
    | Qual_var_type (var, qual) -> Pr.dprintf "variable's type should not be qualified (qual is %s): %s" (Qual.string_of_qual qual) (string_of_var var)

  (* - - - info from above - - - *)
  (* The information that moves down the walk (from above) is a record
     that consists of variable, type and label environments and an
     expected return type.  

     Why does the return type (ret_typ) move down the walk, rather
     than up?  Note that we are not doing type-reconstruction-- we
     know the type(s) we expect each graph region to return a priori.
     For functions we use their return type (stored with the function
     variable); for cuts, we use the argument type(s) of the cut
     label. 
  *)
  type above = {
    env_vars : Vars.t ;
    env_typs : Typs.t ;
    env_labs : typs Labs.t ;
    ret_typs : typs ;
  }
      
  (* - - - info from below - - - *)
  (* The information that moves back up the walk (from below) is just
     a list of errors: *)

  type below = {
    errs : (err * G.ppt) list -> (err * G.ppt) list ;
    dumm : unit ;
  }

  let below_of_above above =
    { errs = (fun es -> es) ; dumm = () }

  let no_error : below -> below = 
    fun below -> below
      
  let error pt err : below -> below = 
    fun below -> { below with errs = 
        fun es -> (err, pt) :: (below.errs es) }

  let merge_below (b1:below) (b2:below) = 
    {b1 with errs = (fun es -> b1.errs (b2.errs es))}

  (* - - - Utility combinators - - - *)

  (* Add a variable to environment *)
  let add_var pt uniq var above : above * (below -> below) =    
    { above with env_vars = Vars.add var above.env_vars },
    ( let dup_error =
        if (var <> Temps.wildcard_var) && uniq && (Vars.mem var above.env_vars)
        then error pt (Duplicate_var var) 
        else no_error
      in
      let qual_error =
        let q' = Qual.qual_of_type var.Cil.vtype in
        match q' with
          | Some Qual.Foreign_c -> no_error
          | Some q              -> error pt (Qual_var_type(var, q))
          | None                -> no_error
      in
      (fun b -> dup_error (qual_error b))
    )

  (* Add a list of variables to environment *)
  let add_vars pt vars : above -> above * (below -> below) =
    G.Walk.fold (add_var pt true) vars
      
  (* Add a basic-block label to the environment *)
  let add_label pt label typs above : above * (below -> below) =
    { above with env_labs = Labs.add label typs above.env_labs },
    if not (Labs.mem label above.env_labs)
    then no_error
    else error pt (Duplicate_label label)


  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)
  (* Static Semantics -- Binding. *)      
  (* -- Check that various things are closed. *)

  (* Emit an error for each variable in fvs *)
  let check_no_free_vars above pt (fvs:Vars.t) : below -> below =
    let diff = Vars.diff fvs above.env_vars in 
    if Vars.is_empty diff 
    then no_error
    else Vars.fold (fun v -> error pt (Unbound_var v)) diff

  (* Check that a pure has no free variables (outside the variable environment) *)
  let check_pure_is_closed above pt pure : below -> below = 
    check_no_free_vars above pt (G.Free_vars.fv_of_pure pure)

  (* Check that a pure has no free variables (outside the variable environment) *)
  let check_pures_are_closed above pt pures : below -> below = 
    List.fold_right (check_pure_is_closed above pt) pures

  (* Check that an lval has no free variables (outside the variable environment) *)  
  let check_lval_is_closed above pt lval : below -> below = 
    check_no_free_vars above pt (G.Free_vars.fv_of_lval lval)
      
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)
  (* As a sanity check, and to provide useful and accurate error
     messages, we try to keep straight the types of holes (expression or
     value contexts) and the types of things filling these holes (the
     expressions going into the context) *)

  type 'a hole = Hole of 'a
  let unhole (Hole a) = a
  let hole a = Hole a

  type 'a fill = Fill of 'a    
  let unfill (Fill a) = a
  let fill a = Fill a

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)
  (* Type Equality and Static Semantics. *)      
  (* -- Variables' outer types do not carry any qualifiers. *)
      
  let no_qual : typ -> typ = Qual.type_without_qual
  let no_qualf (Fill t) = Fill (no_qual t)
  let no_qualh (Hole t) = Hole (no_qual t)

  let is_null_ptr pure =
    let t = Cil.typeOf pure in
    ( Cil.typeSig Cil.voidPtrType = Cil.typeSig t )
    && Cil.isZero pure

  let convert_if_null pure ty_ctxt =
    if is_null_ptr pure && Cil.isPointerType ty_ctxt
    then ty_ctxt else Cil.typeOf pure

  let has_implicit_coercion (th : typ hole) (tf : typ fill) : bool = 
    (* TODO -- check that the fill type is coerciable *)
    match (Cil.unrollType (unhole th)), (Cil.unrollType (unfill tf)) with
      | Cil.TInt   _, Cil.TInt _ -> 
          ( Cil.bitsSizeOf (unhole th) > Cil.bitsSizeOf (unfill tf) )

      | Cil.TFloat _, Cil.TFloat _ ->
          ( Cil.bitsSizeOf (unhole th) > Cil.bitsSizeOf (unfill tf) )
          
      (* TODO: should these be permitted? *)
      | Cil.TFloat _, Cil.TInt _   -> true
      | Cil.TInt   _, Cil.TFloat _ -> true
      | _ -> false

  (* Check if two types match:
     
     -- "identical" is a relation defined by the C standard.  It expands
        typedef names to their definitions. But otherwise, It is
        fairly strict (e.g., long not-identical to int).

     -- we allow for implicit coercions (e.g., from long to int)
  *)
  let types_match (Hole th) (Fill tf) : bool =
    (typeSig th) = (typeSig tf)                  (* ===> th is "identical to" tf *)
    || has_implicit_coercion (Hole th) (Fill tf) (* ===> tf is coerceable to th  *)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

  (* Check if a pair of types match *)
  let check_match above pt 
      (th : typ hole)
      (tf : typ fill) : below -> below 
    = 
    if types_match th tf then no_error 
    else error pt (Type_mismatch (unhole th, unfill tf))

  (* Check if a pair of type sequences (type lists) are matching *)
  let check_matches above pt 
      (tsh : (typ hole) list) 
      (tsf : (typ fill) list) : below -> below 
    = 
    let check_two = check_match above pt in
    fun below -> try
      List.fold_right2 check_two tsh tsf below
    with Invalid_argument _ ->
      error pt (Types_mismatch (List.map unhole tsh, List.map unfill tsf)) below

  (* If some predicate holds, then check that two type sequences are matching *)
  let check_matches_if above pt (predicate:bool) 
      (typs1 : (typ hole) list) 
      (typs2 : (typ fill) list) : below -> below 
    =
    if predicate then check_matches above pt typs1 typs2 else no_error
        
  (* Check that a pure is well-typed ("match"). *)
  let check_match_pure above pt typ pure : below -> below = fun b ->
    begin check_pure_is_closed above pt pure 
      **> check_match above pt (Hole typ) (Fill (Cil.typeOf pure))
      **> b end

  (* Check that a sequence of pures is well-typed ("match"). *)  
  let check_match_pures above pt typs pures : below -> below = 
    fun below -> try 
      List.fold_right2 (check_match_pure above pt) typs pures below
    with Invalid_argument _ ->
      error pt (Types_mismatch (typs, List.map Cil.typeOf pures)) below
    
  let check_pure_is_scalar above pt pure : below -> below = 
    if not (Scalar.is_scalar_type (Cil.typeOf pure))
    then error pt (Not_a_scalar pure)
    else no_error

  let check_simp pt simp typ = fun above -> above,
    match simp with
      | G.Pure pure -> check_match above pt (Hole typ) (Fill (convert_if_null pure typ))
      | G.Read lval -> check_match above pt (Hole typ) (no_qualf (Fill (Cil.typeOfLval lval)))
      | G.Peek lval -> check_match above pt (Hole typ) (no_qualf (Fill (Cil.typeOfLval lval)))
          
      | G.Scope -> check_match above pt (Hole typ) (Fill (Cil.voidPtrType))
          
      | G.Alloc typ' -> check_match above pt 
          (Hole typ) (Fill (no_qual (Annot.ptr_type_of_alloc_type typ')))
            
      | G.Poke (lval, pure) -> begin fun below ->
          let pure_typ = convert_if_null pure (no_qual (Cil.typeOfLval lval)) in
          begin check_match above pt (Hole typ) (Fill (Cil.voidType))
            **> check_match above pt (Hole (no_qual (Cil.typeOfLval lval))) (Fill pure_typ)
            **> below end
        end
          
      | G.Low (_, pures) -> 
          (* TODO: for now I just check that these pures are all closed *)
          List.fold_right (check_pure_is_closed above pt) pures

      | G.Ffi (_, _, pures) ->
          (* TODO: for now I just check that these pures are all closed *)
          List.fold_right (check_pure_is_closed above pt) pures

      | G.Clib (pure, pures) -> begin fun below ->
          (* TODO: for now I just check that these pures are all closed *)
          begin check_pure_is_closed above pt pure
            **> check_pures_are_closed above pt pures
            **> below end
        end

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* Type-check a CFG. *)
  let check_graph (graph:G.t) above : below =
    G.Walk.walk {
      G.Walk.walk_first = begin fun first above ->
        match first with
          | G.Entry -> above, no_error
          | G.Label (_, _, vars, pt) ->
              (* Note: label already added at corresponding Let_block node *)
              add_vars pt vars above
      end (* walk_first *);
      
      G.Walk.walk_nested = begin fun labels above -> 
        (above, List.fold_right merge_below)
      end (* walk_nested *);

      G.Walk.walk_middle = begin fun middle above ->
        
        (* type-check a scope annotation. *)
        let check_scope above pt scope = match scope with
          | G.Scope_same        -> no_error
          | G.Scope_change pure -> check_match_pure above pt
              (Cil.voidPtrType) (* <-- TODO: is this the official type of a scope?. *)
                (pure)
        in
        
        (* type-check a memo annotation. *)
        let check_memo above pt memo = match memo with
          | G.Memo_no              -> no_error
          | G.Memo_yes (pt, scope) -> check_scope above pt scope 
        in        
        
        (* type-check the middle node. *)
        match middle with

          | G.Cut (label, memo, pt) ->
              (* Find the label, it's argument type(s) become the
                 return type of this Cut's body.*)
              if Labs.mem label above.env_labs then
                ( {above with 
                     env_labs = Labs.empty ;
                     ret_typs = Labs.find label above.env_labs }, 
                  check_memo above pt memo )
              else 
                (* If we can't find the label (it's not in scope) we
                   don't know the return type(s), so we fudge it -- hey,
                   there's at least one error already right? *)
                ( {above with 
                     env_labs = Labs.empty ;
                     ret_typs = [] }, 
                  error pt (Unbound_label label) )

          | G.Memo (scope, pt) ->
              above, check_scope above pt scope
                
          | G.Update pt ->
              above, no_error

          | G.Rvars (rvars, pt) ->
              above, check_pures_are_closed
                above pt (List.map Abbrev.pure_of_var rvars)

          | G.Let_block (labels, pt) -> 
              G.Walk.fold begin fun label ->
                let (_, _, vars, _) = G.info_of_label label graph in
                let typs = List.map (fun v -> v.Cil.vtype) vars in
                add_label pt label typs 
              end labels above
                
          | G.Write (lval, pure, pt) -> above, fun below ->
              let lval_ty = Cil.typeOfLval lval in
              begin check_lval_is_closed above pt lval 
                **> check_match above pt 
                ( Hole ( no_qual lval_ty ))
                ( Fill ( convert_if_null pure ( no_qual lval_ty ) ))
                **> below end
                
            | G.Let_simp (var, simp, pt) -> 
                begin add_var    pt true var                     
                  @@> check_simp pt simp var.Cil.vtype
                end above
                  
      end (* walk_middle *) ;
      
      G.Walk.walk_last = begin fun last above ->
        let check_br above pt br : below -> below = match br with
          | G.Br (label, pures) ->
              if Labs.mem label above.env_labs then
                check_match_pures above pt (Labs.find label above.env_labs) pures 
              else
                error pt (Unbound_label label)
        in
        match last with
          | G.Exit -> (below_of_above above)

          | G.Branch (br, pt) ->
              check_br above pt br 
                (below_of_above above)

          | G.Return (pures, pt) ->
              check_match_pures above pt above.ret_typs pures 
                (below_of_above above)
                
          (* TEMP / Fix this more cleanly. *)
          (* Variadic functions -- e.g., printf *)
          | G.Call (G.Default, pure, pures, pt) 
            when begin match Cil.typeOf pure with 
              | Cil.TFun (_, _, true, _) -> true  (* is  var arg *)
              | _                        -> false (* not var arg *)
            end -> begin match pure with
              | Cil.Lval (Cil.Var v, Cil.NoOffset) -> 
                  let (_, _, rts) = Annot.arrow_type_unpack v.Cil.vtype in
                  (* Same as general call case below, except that we don't
                     check the type of the arguments, only that they are closed. *)
                  begin check_pures_are_closed above pt pures
                    **> check_matches       above pt 
                    (List.map hole above.ret_typs) 
                    (List.map fill rts)
                    **> below_of_above above end
              | _ -> 
                  error pt (Not_a_func_var pure)
                    (below_of_above above)
            end


          | G.Call (mode, pure, pures, pt) ->
              begin match pure with
                | Cil.Lval (Cil.Var v, Cil.NoOffset) -> 
                    let (_, ats, rts) = Annot.arrow_type_unpack v.Cil.vtype in
                    let pures_types = 
                      if (List.length pures) = (List.length ats) then
                        (List.map2 convert_if_null pures ats)
                      else
                        List.map Cil.typeOf pures
                    in 
                    begin check_matches      above pt (List.map hole ats) (List.map fill pures_types)
                      **> check_matches      above pt (List.map hole above.ret_typs) (List.map fill rts)
                      **> check_matches_if   above pt (mode = G.Run_core) [] (List.map fill rts)
                      **> below_of_above above end
                      
                | _ -> 
                    error pt (Not_a_func_var pure)
                      (below_of_above above)
              end
                
          | G.Cond (pure, br1, br2, pt) ->
              begin check_pure_is_closed above pt pure 
                **> check_pure_is_scalar above pt pure
                **> check_br             above pt br1 
                **> check_br             above pt br2 
                **> below_of_above       above end
      end ;
    } above (G.entry graph)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  let typ_program above (program:P.t) : below = 
    P.Walk.walk {
      P.Walk.walk_glob = begin fun glob above -> 
        match glob with
          | P.Mark      _  -> above, no_error          
          
          | P.Vard (v, pt) -> add_var pt false v above
          
          | P.Var  (v, simp, pt) ->
              begin add_var    pt false v 
                @@> check_simp pt simp  v.Cil.vtype
              end above

          | P.Func (v, pt)  -> add_var pt false v above
              
          (* TEMP / TODO -- we need this case to support trace node
             descriptor definitions (which carry initializations) *)
          | P.Cilg (Cil.GVar (v, _, _), pt) -> add_var pt false v above

          | P.Cilg (g, pt) -> begin match g with
                | ( Cil.GType    _
                  | Cil.GCompTag _
                  | Cil.GEnumTag _
                  | Cil.GEnumTagDecl _
                  | Cil.GCompTagDecl _
                  | Cil.GText   _
                  | Cil.GPragma _ )
                  -> (* TODO: add types to type environment.? *)
                  above, no_error 
              
                | Cil.GFun _ -> 
                    (* Function should all be a [Program.Func] *)
                    assert false

                | Cil.GVar _ ->
                    (* Variables should all be a [Program.Var] *)
                    assert false
                    
                | Cil.GVarDecl _ ->
                    (* Should be represented as a GVar with a separate initializer? *)
                    (* TODO: We haven't thought about globals too much as of yet. *)
                    assert false

                | Cil.GAsm _ -> 
                    Printf.eprintf "bad global: %s\n" (string_of_global g) ;
                    assert false
            end (* match Cil global *)
      end (* walk_glob *);
      
      P.Walk.walk_func = begin fun (var, vars, graph, pt) above ->
        (* get return type; check that the argument types match. *)
        let (_, ats, rts) = Annot.arrow_type_unpack var.Cil.vtype in
        (* TODO-Nov-23-2009: 
           Check that the arg types (ats) match the var types of vars *)
        let below = (
          (* add parameters to environment;
             specify the expected return type(s). *)
          begin add_vars pt vars
            @@> (fun a -> {a with ret_typs = rts}, no_error)
          end
            
          (* Under extended environments, type-check the graph. *)
          @!> check_graph graph
        ) above (* <-- top-level environment *) 
        in
        (* top-level environment unchanged; 
           add any errors from type-check of func graph. *)
        above, 
        (fun b -> merge_below below b)
      end (* walk_func *) ;
      
      (* Check the root graph. *)
      P.Walk.walk_root = check_graph    

    } above program
          
  let of_program (program:P.t) = 
   let below = 
      let above = { 
        env_vars = Vars.empty ;
        env_typs = Typs.empty ;
        env_labs = Labs.empty ;
        ret_typs = [] }
      in typ_program above program
    in 
    (* Get the errors, print them. *)
    let errs = ( below.errs [] ) in
    if errs <> [] then begin
      Pr.fprint stderr ~width:80 
        (Pr.dprintf "@!Found %d error%s while type-checking the intermediate code:@!" 
           (List.length errs) (match errs with [_] -> "" | _ -> "s"))
      ;
      List.iter begin fun (err, pt) -> 
        Pr.fprint stderr ~width:80 
          (Pr.dprintf "Ceal Error %a: %a@!" 
             Program_point.long_doc pt (fun _ -> d_err) err)
      end errs ; 
      Printf.fprintf stderr "\n%!" ;
      false
    end
    else 
      true
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Lower_verifier = struct
  module L   = Low
  module G   = Zipcfg
  module B   = G.Build
  module P   = Program
  module Ppt = Program_point
      
  let null : G.pure = Cil.mkCast ~e:Cil.zero ~newt:Cil.voidPtrType 

  let wc_var = Temps.wildcard_var

  let lower_graph above (graph:G.t) : G.zt =
    G.Walk.walk
      { G.Copy.walk_fns with          
          G.Walk.walk_middle = begin fun middle above -> match middle with              

            (* Lower allocations *)
            | G.Let_simp (var, G.Alloc typ, ppt) ->
                above, B.let_simp ppt var
                  (G.Low (Low.Alloc_invoke, [null; null; Cil.SizeOf typ]))

            (* Erase scopes (no memoization ==> no scopes needed) *)
            | G.Let_simp (var, G.Scope, ppt) ->
                above, B.let_simp ppt var (G.Pure null)

            (* Lower reads (into peeks) *)
            | G.Let_simp (var, G.Read lval, ppt) ->
                above, B.let_simp ppt var (G.Peek lval)
                  
            (* Lower writes (into pokes) *)
            | G.Write (lval, pure, ppt) ->
                above, B.let_simp ppt wc_var (G.Poke (lval, pure))
                  
            (* Erase memo points. *)
            | G.Memo _ -> above, B.cons_id
            
            (* Erase memo points. *)
            | G.Cut (label, _, ppt) -> above,
                B.cut ppt label G.Memo_no

            (* No other changes: *)
            | _ -> G.Copy.walk_fns.G.Walk.walk_middle middle above

          end ; 
          
          G.Walk.walk_last = begin fun last above -> match last with              
            (* Calling the core program.  
               We handle this by wrapping it with some RT calls *)
            | G.Call (G.Run_core, pure, pures, ppt) ->
                B.call ppt G.Default pure pures G.emptyz

            (* No other changes: *)
            | _ -> G.Copy.walk_fns.G.Walk.walk_last last above
          
          end ;
      }
      above (G.entry graph)

  let walk_program above (program:P.t) : Program.comb =
    P.Walk.walk
      { P.Copy.walk_fns with 
          
          P.Walk.walk_glob = begin function
            | P.Var (var, simp, ppt) -> begin 
                let simp' = match simp with
                  | G.Alloc typ -> (G.Low (Low.Alloc_invoke, [null; null; Cil.SizeOf typ]))
                  | _           -> assert false
                in
                fun a -> a,
                  fun comb z -> P.Build.var ppt var simp' **> comb **> z
              end
              
            | glob -> 
                P.Copy.walk_fns.P.Walk.walk_glob glob

          end ;

          P.Walk.walk_func = begin (* Lower function bodies. *)
            fun (fun_var, fun_vars, fun_graph, fun_ppt) above ->
              above, begin fun comb z ->
                P.Build.func'' fun_ppt begin
                  fun_var, fun_vars, 
                  G.unfocus (lower_graph () fun_graph), (* lower the graph. *)
                  G.Ppt.from fun_ppt
                end **> comb **> z
              end
          end ; 
      } above program

  let of_program (program:P.t) : Program.comb =
    walk_program () program

end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Program_levels = struct
  
  module Callgraph : sig 
    type t
      
    type fnnd = 
      | Root               (* Unique & abstract. *)
      | Meta of Zipcfg.var (* A meta-level function *)
      | Core of Zipcfg.var (* A core-level function *)
      | Conv of Zipcfg.var (* Conventional C code. *)
          
    val of_program : Program.t -> t
      
    val find   : fnnd -> t -> fnnd list
    val close  : fnnd -> t -> fnnd list
    val fold   : (fnnd -> fnnd -> 'a -> 'a) -> 'a -> t -> 'a
    val dump   : string -> t -> unit
  end
    = 
  struct
    module G = Zipcfg
    module P = Program
      
    type fnnd = 
      | Root          (* Unique & abstract. *)
      | Meta of G.var (* A meta-level function *)
      | Core of G.var (* A core-level function *)        
      | Conv of G.var (* Conventional C function; 
                         these are always _external_ to our 
                         program representation & compilation. *)
          
    module Fnnd = struct 
      type t = fnnd
      let compare_vars f g = compare f.Cil.vid g.Cil.vid 
      let compare a b = match (a,b) with
        | Root,   Root   -> 0
        | Meta f, Meta g -> compare_vars f g
        | Core f, Core g -> compare_vars f g
        | Conv f, Conv g -> compare_vars f g
        | a     , b      -> Pervasives.compare a b
    end
      
    (* - - - - - - - - - - - - - - - - - - - - - - - - *)
      
    module V = Varinfo.Set
      
    type above = { glob_vars : V.t  ;
                   fnnd_ctxt : fnnd_ctxt ; }
        
    and fnnd_ctxt = 
      | Only_root
      | Meta_and_core of G.var
          
    module F = SetMap(Set.Make(Fnnd))(Map.Make(Fnnd))
      
    type below = { callgraph : F.t ;
                   dummy     : unit ; }
        
    let below_empty = 
      { callgraph = F.empty [] ;
        dummy     = (); }
        
    type t = F.t
        
    let ( **> ) f g = f g
      
    (* Compute the call graph for the given program. *)
    let of_program (program:P.t) : t = 
      
      (* Is fun_var defined witin our program('s compilation unit)? If
         not, we will treat fun_var as a conventional function. *)
      let is_defined_in_program (fun_var:G.var) =
        let (_, funcs, _) = program in
        P.Funcs.mem fun_var funcs
      in
      
      (* Selectively adds call-edge(s) to the call graph. *)
      let add (mode:G.mode) (fun_var:G.var) (above:above) = fun (below:below) ->
        (* Sanity check: Is variable properly scoped? *)
        let _ = if (not (V.mem fun_var above.glob_vars)) then
          E.error "%s: no prototype or definition" fun_var.Cil.vname
        in
        (* Is the variable defined in the program's compilation unit *)
        let add_call fnnd1 fnnd2 = fun below ->
          {below with callgraph = F.extend fnnd1 fnnd2 below.callgraph} 
        in
        match above.fnnd_ctxt, mode, is_defined_in_program fun_var with          
            
          (* Sanity check: all calls into core must be to functions
             defined within the program that we have access to.*)
          | _, G.Run_core, false -> assert false
              
          (* Calls from root *)
          | Only_root, G.Default,  true  -> add_call Root (Meta fun_var) below
          | Only_root, G.Default,  false -> add_call Root (Conv fun_var) below
          | Only_root, G.Run_core, true  -> add_call Root (Core fun_var) below
              
          (* Calls from within a function body *)
          | Meta_and_core src_var, G.Default, true -> 
              begin add_call (Meta src_var) (Meta fun_var) 
                **> add_call (Core src_var) (Core fun_var)               
                **> below end
                
          (* A call that crosses from meta-level to core level. *)
          | Meta_and_core src_var, G.Run_core, true -> 
              begin add_call (Meta src_var) (Core fun_var)
                **> add_call (Core src_var) (Core fun_var)
                **> below end
                
          | Meta_and_core src_var, G.Default, false ->
              begin add_call (Meta src_var) (Conv fun_var)
                **> add_call (Core src_var) (Conv fun_var)
                **> below end
      in

      (* For the case where a variable use (of function type or
         otherwise) appears outside a call site; We assume that the
         variable can escape, and hence be called from an unknown
         context (represented here by Root). *)
      let var_uses (vars:V.t) (above:above) = fun (below:below) -> 
        (* V.fold begin fun var below ->
           add G.Default var {above with fnnd_ctxt = Only_root} below 
           end vars *) below 
      in
      
      (* Merge belows (i.e., merge callgraphs) *)
      let merge_below b1 b2 = 
        {b1 with callgraph = F.merge b1.callgraph b2.callgraph}
      in

      (* Generate a callgraph for a CFG *)
      let walk_graph above graph : below =
        G.Walk.walk {        
          G.Walk.walk_first = begin 
            fun _ above -> above, fun below -> below (* NO-OP. *) 
          end ;
          
          G.Walk.walk_middle = begin fun middle above -> above, 
            var_uses (G.Free_vars.fv_of_middle middle) above
          end ;

          G.Walk.walk_nested = begin fun _ above ->
            (above, List.fold_right merge_below)
          end ;

          G.Walk.walk_last = begin fun last above -> 
            match last with              
                (* A first-order call-site; we know the (function) variable being called. *)
              | G.Call (mode, (Cil.Lval (Cil.Var fun_var, Cil.NoOffset)), pures, _) ->
                  begin (if V.mem fun_var above.glob_vars 
                         then add mode fun_var above else fun b -> b)
                    **> var_uses (G.Free_vars.fv_of_pures pures) above
                    **> below_empty end
                    
              | last -> 
                  (* No ("direct") calls here, only ("indirect") uses *)
                  begin var_uses (G.Free_vars.fv_of_last last) above 
                    **> below_empty end
          end ;
        } (* end walk_fns *) above graph
      in
      (fun below -> below.callgraph) **>
        P.Walk.walk {
          P.Walk.walk_glob = begin fun glob above -> 
            match glob with
              | P.Mark _ -> (above, fun b -> b)
                  
              | P.Vard (v, _) -> begin
                  ( if Cil.isFunctionType v.Cil.vtype 
                    then { above with glob_vars = V.add v above.glob_vars }
                    else above ), 
                  fun b -> b
                end

              | P.Var (v, _, _) ->
                  assert (not (Cil.isFunctionType v.Cil.vtype)) ;
                  begin above, fun b -> b end
                  
              | P.Func (v, _) -> begin
                  { above with glob_vars = V.add v above.glob_vars }, 
                  fun b -> b
                end
                  
              | P.Cilg _ -> begin above, fun b -> b end 
          end ;
          
          P.Walk.walk_func = begin fun (var, _, graph, _) above ->
            (* Every function that is not static will have a meta-level
               version ('reachable' from the root-- unknown code we
               later link against). *)
            let has_edge_from_root v = v.Cil.vstorage != Cil.Static in
            (* We add edges from root (if appropriate), and walk the
               function body itself adding call edges appropriately *)
            above, fun below -> merge_below 
              (if has_edge_from_root var 
               then add G.Default var {above with fnnd_ctxt = Only_root} below
               else below)
              (walk_graph 
                 {above with fnnd_ctxt = Meta_and_core var} 
                 (G.entry graph))
          end ;
          
          P.Walk.walk_root = begin fun graph above -> 
            (walk_graph 
               {above with fnnd_ctxt = Only_root } 
               (G.entry graph))
          end
        } 
        { glob_vars = V.empty ; fnnd_ctxt = Only_root} 
        program

    let find fnnd callgraph = F.S.elements (F.get fnnd callgraph)

    (* Fold f over each edge of the callgraph, start with a. *)
    let fold f a callgraph =
      F.M.fold begin fun fnnd1 calls a ->
        F.S.fold begin fun fnnd2 a -> 
          f fnnd1 fnnd2 a
        end calls a
      end callgraph a

    let close fnnd callgraph = 
      let (++) = F.S.union in
      let (--) = F.S.diff in
      let rec loop grey black =
        if F.S.is_empty grey then black
        else let f = F.S.choose grey in
        loop ((F.S.remove f grey) 
              ++ ((F.get f callgraph) 
                  -- black))
          (F.S.add f black)
      in F.S.elements
           (loop (F.S.singleton fnnd) F.S.empty)
           
    (* Convenience/Debugging module for dumping the callgraph to disk. *)
    module Dump = struct
      let string_of_fnnd : fnnd -> string = function
        | Root   -> "root"
        | Meta v -> Printf.sprintf "meta(%s)" v.Cil.vname
        | Core v -> Printf.sprintf "core(%s)" v.Cil.vname          
        | Conv v -> Printf.sprintf "conv(%s)" v.Cil.vname

      let dump_log callgraph : unit =
        fold begin fun fnnd1 fnnd2 _ ->
          ignore (log "%s -> %s@!" (string_of_fnnd fnnd1) (string_of_fnnd fnnd2))
        end () callgraph

      let dump_dot name callgraph : unit =
        let nd_label = function
          | Root   -> "Root"
          | Meta v -> v.Cil.vname
          | Core v -> v.Cil.vname
          | Conv v -> v.Cil.vname
        in
        let nd_name = function
          | Root   -> "root"
          | Meta v -> Printf.sprintf "meta_%s" v.Cil.vname
          | Core v -> Printf.sprintf "core_%s" v.Cil.vname
          | Conv v -> Printf.sprintf "conv_%s" v.Cil.vname
        in
        let out = open_out (name ^ ".dot") in
        let root_closure = List.fold_left begin fun closure fnnd -> 
          F.S.add fnnd closure end (F.S.singleton Root) (close Root callgraph) 
        in
        Printf.fprintf out "digraph {\n" ;
        Printf.fprintf out "center=true;\n" ; 
        let pre, meta, core, conv, post = fold begin (* for each edge .. *)
          fun fnnd1 fnnd2 (pre, meta, core, conv, post) ->
            let this_edge _ = 
              let label, dir, weight, headport, tailport = 
                match fnnd1, fnnd2 with
                  | ( (Meta f, Meta g) 
                    | (Core f, Core g)) 
                      when f == g  -> "\"\"",  "back", "0.25", "s", "n"
                  | Meta _, Core _ -> "\"*\"", "forward", "1", "n", "s"
                  | _              -> "\"\"",  "forward", "1", "n", "s"
              in            
              Printf.fprintf out "%s -> %s [color=\"%s\","
                (nd_name fnnd1) (nd_name fnnd2) 
                (if F.S.mem fnnd1 root_closure && F.S.mem fnnd2 root_closure 
                 then "black" else "grey") ;
              Printf.fprintf out "label=%s, dir=%s, weight=%s, headport=%s, tailport=%s];\n" 
                label dir weight headport tailport
            in
            (pre, meta, core, conv, (fun _ -> this_edge (); post ()))
        end begin 
          F.S.fold begin (* for each node, augment the appropriate cluster *)
            fun fnnd (pre, meta, core, conv, post) ->
              let this_node _ =
                let color, fontcolor = 
                  if F.S.mem fnnd root_closure 
                  then "black", "black" 
                  else "grey", "darkslategrey"
                in
                Printf.fprintf out "%s [label=%s, color=%s, fontcolor=%s, %s];\n"
                  (nd_name fnnd) (nd_label fnnd) color fontcolor
                  "fontsize=10, width=0.3, height=0.2 shape=box"
              in
              match fnnd with
                | Root   -> ((fun _ -> pre (); this_node ()), meta, core, conv, post)
                | Meta v -> (pre, (fun _ -> meta (); this_node ()), core, conv, post)
                | Core v -> (pre, meta, (fun _ -> core (); this_node ()), conv, post)
                | Conv v -> (pre, meta, core, (fun _ -> conv (); this_node ()), post)
          end begin (* all the fnnds in the callgraph *)
            fold begin fun fnnd1 fnnd2 fnnds -> 
              F.S.add fnnd1 (F.S.add fnnd2 fnnds) end F.S.empty callgraph
          end begin (* the preamble for each cluster *)
            let noop _ = () in
            let label l c _ = Printf.fprintf out "label=\"%s\"; color=\"%s\";\n" l c ; in 
            noop, label "Meta Level" "", label "Core Level" "pink", label "External" "blue", noop
          end
            
        end callgraph
        in
        pre () ;
        let do_cluster name body = 
          Printf.fprintf out "subgraph cluster_%s {\n" name ; 
          body () ; 
          Printf.fprintf out "}\n"
        in
        do_cluster "meta" meta ;
        do_cluster "core" core ;
        do_cluster "conv" conv ;
        post () ;
        close_out out ;        
        Dot_util.run name "ps"  "ps" ;
        Dot_util.run name "pdf" "pdf"
    end

    let dump name callgraph = 
      let path_wo_ext = (Filename.concat (!Global_flags.output_path) name) in
      if ! Global_flags.debug_passes then
        Dump.dump_dot path_wo_ext callgraph

  end

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)      
  (* FIX: This module serves to fix a technical problem with the
     region-analysis and destination-passing style: That conventional
     C code cannot be DPS-transformed (since, by def, we don't have it
     in our hands).  

     To address this, before doing the region analysis and DPS
     transformation, we put each call to conventional C code into its
     own region, effectively making these tail-calls into calls by
     introducing a new region especially dedicated to making the
     conventional call.  Later on these can be simplified away whenver
     the enclosing function is not itself DPS-transformed. *)
  module Cut_conv_calls = struct
    module Ppt = Program_point
    module G  = Zipcfg
    module B  = G.Build
    module W  = G.Walk
    module P  = Program
    module Cg = Callgraph

    let lval_of_var   var = (Cil.Var var, Cil.NoOffset)
    let pure_of_lval lval = Cil.Lval lval
    let pure_of_var   var = pure_of_lval (lval_of_var var)

    type context =
      | Context_no_cut (* Base case: No cuts yet. *)
      | Context_of_fresh_cut  (* Special case: Just entered a new cut. *)
      | Context_of_shared_cut (* Ordinary case: In a cut after other code. *)

    let of_program (callgraph:Cg.t) (program:P.t) : Program.comb =
      
      let walk_graph (graph:G.t) =
        W.walk { 
          G.Copy.walk_fns with
            W.walk_middle = begin fun mid context ->
              match mid, context with
                
                | G.Cut _, _ -> 
                    G.Copy.walk_fns.W.walk_middle mid Context_of_fresh_cut

                | _ , _ -> 
                    G.Copy.walk_fns.W.walk_middle mid Context_of_shared_cut
            end ;

            W.walk_last = begin function
              | (G.Call (mode, f, args, ppt)) as last -> begin 
                  begin fun context -> match f with
                    | (Cil.Lval (Cil.Var var, Cil.NoOffset)) ->
                        if ((* Can we avoid this transformation? *)
                          not (P.func_var_is_foreign_c program var) 
                          || context = Context_of_fresh_cut 
                        )
                        then 
                          (* No need to apply transformation. *)
                          G.Copy.walk_fns.W.walk_last last context
                        else 
                          (* Apply the transformation. *)                          
                          let lab_vars, lab_args =
                            match Annot.arrow_type_unpack var.Cil.vtype with
                              | None, _, [ ] -> [], []                              
                              
                              | None, _, [t] -> 
                                  let v = Temps.fresh_var t in 
                                  [v], [pure_of_var v]
                                    
                              | _ -> assert false
                            in
                          let label = Temps.fresh_label (Ppt.loc ppt) in
                          begin 
                            B.let_block ppt label lab_vars
                              (B.return ppt lab_args)
                            **> B.cut ppt label G.Memo_no
                            **> B.call ppt (G.Default) f args
                            **> G.emptyz
                          end

                    | _ ->
                        (* We only support first-order programs right now. *)
                        assert false
                  end
                end
              
              | last -> G.Copy.walk_fns.W.walk_last last
            end
        } Context_no_cut (G.entry graph)
      in
      let walk_program (program:P.t) : Program.comb =
        P.Walk.walk
          { P.Copy.walk_fns with 
              P.Walk.walk_func = begin (* Walk over function bodies. *)
                fun (fun_var, fun_vars, fun_graph, fun_ppt) above ->
                  above, begin fun comb z ->
                    P.Build.func'' fun_ppt begin
                      fun_var, fun_vars, 
                      G.unfocus (walk_graph fun_graph), 
                      G.Ppt.from fun_ppt
                    end **> comb **> z
                  end
              end ; 
          } () program
      in
      walk_program program

  end (* Cut_conv_calls module *)

  module G  = Zipcfg
  module P  = Program
  module Ppt = Program_point
  module Cg = Callgraph
  module V  = Varinfo

  let ( **> ) f x = f x

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* Separate the meta & core levels into separate programs.  We do
     this carefully (using markers) such that we can later merge the
     separated programs back into a single program. *)
  module Separate = struct
    
    type level = 
      | Meta
      | Core

    type above = { fun_level : level }

    type below = { meta_comb : P.comb ;
                   core_comb : P.comb ;
                   dummy_blw : unit   }

    let empty_below = 
      { meta_comb = P.Build.empty ;
        core_comb = P.Build.empty ;
        dummy_blw = () }

    let of_program (callgraph:Cg.t) (program:P.t) : P.comb * P.comb =
      
      (* The program's callgraph, and a list reachable functions. *)
      let _         = Cg.dump "callgraph" callgraph in
      let reachable = Cg.close Cg.Root callgraph in

      (* Choose distinct, core-level versions of function variables. *)
      let meta_vars, core_vers = List.fold_left begin 
        fun (meta_vars, core_vers) fnnd -> match fnnd with
          | Cg.Core var -> (meta_vars, V.Map.add var (Temps.fresh_ver var) core_vers)
          | Cg.Meta var -> (V.Set.add var meta_vars, core_vers)
          | _           -> (meta_vars, core_vers)
      end (V.Set.empty, V.Map.empty) reachable in
                          
      (* Walk over a function's graph, redirecting calls as necessary. *)
      let walk_graph above (graph:G.t) : G.zt =
        G.Walk.walk 
          { G.Copy.walk_fns with
              G.Walk.walk_last = begin fun last above -> match last with                  
                (* Redirect function calls to new core versions where appropriate. *)
                | G.Call (mode, (Cil.Lval (Cil.Var fun_var, Cil.NoOffset)), pures, ppt) ->
                    let mode', fun_var' =
                      match above.fun_level, mode, V.Map.mem fun_var core_vers with
                        | _   , G.Run_core, false -> assert false (* broken invariant. *)
                        | Meta, G.Default,  _     -> G.Default,  fun_var
                        | Meta, G.Run_core, true  -> G.Run_core, V.Map.find fun_var core_vers
                        | Core, G.Default,  true  -> G.Default,  V.Map.find fun_var core_vers
                        | Core, G.Run_core, true  -> G.Default,  V.Map.find fun_var core_vers
                        | Core, G.Default,  false -> G.Default,  fun_var (* fun is conventional. *)
                    in
                    G.Build.call ppt mode'
                      (Cil.Lval (Cil.Var fun_var', Cil.NoOffset)) pures
                      G.emptyz

                | _ -> G.Copy.walk_fns.G.Walk.walk_last last above
              end ; } (* end walk_fns *)
          above (G.entry graph)
      in
          
      let walk_program above (program:P.t) : below =
        
        let comb_emp = P.Build.empty in
        
        (* Insert code into both levels, preceeded by a fresh mark
           (carrying the given P.level, either Meta, Core or Both) *)
        let insert (level:P.level) (meta:P.comb) (core:P.comb) = fun (below:below) -> 
          let mark_comb = P.Build.mark level in
          { below with 
              meta_comb = begin fun z -> mark_comb **> meta **> below.meta_comb **> z end ;
              core_comb = begin fun z -> mark_comb **> core **> below.core_comb **> z end }
        in

        P.Walk.walk
          { P.Walk.walk_glob = begin fun glob above -> above,
              match glob with
                | P.Mark (mark, level) ->
                    let comb = P.Build.mark' mark level in
                    insert P.Both comb comb

                | P.Var (var, simp, ppt) ->
                    let comb = P.Build.var ppt var simp in
                    insert P.Both comb comb

                | P.Vard (var, ppt) ->
                    let comb var = P.Build.vard ppt var in                    
                    let core_ver var =  V.Map.find var core_vers in 
                    let desc, meta, core = 
                      match (Cil.isFunctionType var.Cil.vtype,
                             V.Set.mem var meta_vars, 
                             V.Map.mem var core_vers) 
                      with
                        | false, _,    _     -> (`Same, comb var, comb var) (* not a function. *)
                        | true, false, false -> (`Same, comb var, comb var) (* conventional fun. *)
                        | _,    true,  false -> (`Meta, comb var, comb_emp) (* meta only. *)
                        | _,    false, true  -> (`Core, comb_emp, comb (core_ver var)) (* core only. *)
                        | _,    true,  true  -> (`Diff, comb var, comb (core_ver var)) (* both. *)
                    in
                    begin match desc with
                      | `Same -> insert P.Both meta core
                      | `Meta -> insert P.Meta meta core
                      | `Core -> insert P.Core meta core
                      | `Diff -> begin fun (below:below) ->
                          begin insert P.Meta meta comb_emp
                            **> insert P.Core comb_emp core
                            **> below end
                        end
                    end

                | P.Func (func, pp) -> 
                    (fun b -> b) (* See [walk_func] defined below. *)

                | P.Cilg (cilg, ppt) ->
                    let comb = P.Build.cil ppt cilg in
                    insert P.Both comb comb
            end ;
            
            P.Walk.walk_func = begin fun func above -> 
              let (fun_var, fun_vars, fun_graph, fun_ppt) = func in
              (* If in core level, create a prototype (for meta level)
                 and a new body (for core level) *)
              let core_proto, core_func = match V.Map.mem fun_var core_vers with
                | false -> P.Build.empty, P.Build.empty
                | true  -> 
                    let core_var = V.Map.find fun_var core_vers in
                    P.Build.vard fun_ppt core_var,
                    P.Build.func fun_ppt begin
                      core_var, fun_vars, 
                      G.unfocus (walk_graph {fun_level=Core} fun_graph),
                      G.Ppt.from fun_ppt 
                    end
              in
              (* If in the meta level, create a new body for the function. *)
              let meta = match V.Set.mem fun_var meta_vars with
                | false -> P.Build.empty
                | true  -> P.Build.func fun_ppt begin 
                    fun_var, fun_vars, 
                    G.unfocus (walk_graph {fun_level=Meta} fun_graph),
                    G.Ppt.from fun_ppt
                  end
              in
              above, begin fun (below:below) ->
                begin insert P.Meta (fun z -> core_proto **> meta **> z) comb_emp
                  **> insert P.Core comb_emp (fun z -> core_func  **> z)
                  **> below end 
              end
            end ;
            
            P.Walk.walk_root = begin fun graph above ->
              (* TODO: This part of the program representation is dumb and wrong. *)
              empty_below
            end ;
          } above program
      in
      let walk_out = walk_program () program in
      (walk_out.meta_comb, walk_out.core_comb)
  end

  module Merge = struct    
    exception Unbalanced_mark of P.mark * P.level

    let of_programs (meta_program:P.t) (core_program:P.t) =
      let meta_globs, meta_funcs, meta_root = meta_program in
      let core_globs, core_funcs, core_root = core_program in
      (* TODO: We drop the core_root here ...  This seems reasonable
         since the root code is always in the meta level (and is
         always empty for the core program), though we should assert
         this rather than assuming it. *)
      let comb0 : P.comb = begin fun (z, _, _) -> z,
        P.merge_funcs meta_funcs core_funcs,
        G.nodes_of_graph meta_root
      end in
      let add g (globs:P.comb) : P.comb = 
        fun z -> P.Build.glob g **> globs **> z 
      in
      let rec copy (globs:P.comb) level meta core = 
        match (level, meta, core) with
          | _, (P.Mark (mark1, level1)) :: meta, 
            (P.Mark (mark2, level2)) :: core 
            ->
              if mark1 <> mark2 then assert false 
              else if level1 <> level2 then assert false
              else copy globs level1 meta core

          | P.Meta, (P.Mark (m,l))::_, _ -> raise (Unbalanced_mark (m,l))
          | P.Core, _, (P.Mark (m,l))::_ -> raise (Unbalanced_mark (m,l))
              
          | P.Meta, g::gs, _     -> copy (add g globs) P.Meta gs   core
          | P.Both, g::gs, h::hs -> copy (add g globs) P.Both gs   hs
          | P.Core, _,     g::gs -> copy (add g globs) P.Core meta gs
          | _,      [],    []    -> globs
          | _,      _,     _     -> assert false
      in 

      let rec merge level meta core = 
        let ins g (globs:P.comb) : P.comb = 
          fun z -> P.Build.glob g **> globs **> z 
        in
        match (level, meta, core) with
          | _, (P.Mark (mark1, level1)) :: meta, 
            (P.Mark (mark2, level2)) :: core 
            ->
              if mark1 <> mark2 then assert false 
              else if level1 <> level2 then assert false
              else merge level1 meta core

          | P.Meta, (P.Mark (m,l))::_, _ -> raise (Unbalanced_mark (m,l))
          | P.Core, _, (P.Mark (m,l))::_ -> raise (Unbalanced_mark (m,l))
              
          | P.Meta, g::gs, _     -> ins g (merge P.Meta gs core)
          | P.Both, g::gs, h::hs -> ins g (merge P.Both gs hs  )
          | P.Core, _,     g::gs -> ins g (merge P.Core meta gs)
          | _,      [],    []    -> comb0
          | _,      _,     _     -> assert false
      in 
      merge P.Both meta_globs core_globs
  end
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Lower_meta = struct
  module L  = Low
  module G  = Zipcfg
  module B  = G.Build
  module P  = Program
  module Ppt = Program_point
  
  let pure_of_lval (lval:G.lval) : G.pure = Cil.Lval lval
    
  let null : G.pure = Cil.mkCast ~e:Cil.zero ~newt:Cil.voidPtrType 

  let wc_var = Temps.wildcard_var

  let lower_graph above (graph:G.t) : G.zt =
    G.Walk.walk
      { G.Copy.walk_fns with
          
          G.Walk.walk_middle = begin fun middle above -> match middle with              

            (* Lower allocations *)
            | G.Let_simp (var, G.Alloc typ, ppt) ->
                above, B.let_simp ppt var
                  (G.Low (Low.Alloc_invoke, [null; null; Cil.SizeOf typ]))

            (* Erase scopes (no memoization ==> no scopes needed) *)
            | G.Let_simp (var, G.Scope, ppt) ->
                above, B.let_simp ppt var (G.Pure null)

            (* Lower reads *)
            | G.Let_simp (var, G.Read lval, ppt) ->
                let q, t, ptr = Low.qual_basetype_pointer_of_lval lval in
                above, B.let_simp ppt var
                  (G.Low (Low.Read_invoke (q,t), [null; null; ptr]))
                  
            (* Lower writes *)
            | G.Write (lval, pure, ppt) ->
                let q, t, ptr = Low.qual_basetype_pointer_of_lval lval in
                above, B.let_simp ppt wc_var 
                  (G.Low (Low.Write_invoke (q,t), [null; null; ptr; pure]))
                  
            (* Memos become no-ops. *)
            | G.Memo _ -> above, B.cons_id
            
            (* Memos become no-ops. *)
            | G.Cut (label, _, ppt) -> above,
                B.cut ppt label G.Memo_no

            (* No other changes: *)
            | _ -> G.Copy.walk_fns.G.Walk.walk_middle middle above

          end ; 
          
          G.Walk.walk_last = begin fun last above -> match last with              
            (* Calling the core program.  
               We handle this by wrapping it with some RT calls *)
            | G.Call (G.Run_core, pure, pures, ppt) ->
                let label = Temps.fresh_label (G.Ppt.loc ppt) in
                B.let_block ppt label [] begin 
                  fun z -> B.let_simp ppt wc_var (G.Low (L.Core_end, []))
                    (* Invariant: calls into core always return void. *)
                    **> B.return ppt []
                    **> G.emptyz
                end
                **> B.let_simp ppt wc_var (G.Low (L.Core_begin, []))
                **> B.cut ppt label (G.Memo_no)
                **> B.call ppt G.Default pure pures
                **> G.emptyz
                  
            (* No other changes: *)
            | _ -> G.Copy.walk_fns.G.Walk.walk_last last above
          
          end ;
      }
      above (G.entry graph)

  let walk_program above (program:P.t) : Program.comb =
    P.Walk.walk
      { P.Copy.walk_fns with 
          
          P.Walk.walk_glob = begin function               
            | P.Var(var, G.Alloc typ, ppt) ->
                (fun above -> above, fun comb z ->
                   P.Build.var ppt var 
                     (G.Low (Low.Alloc_invoke, [null; null; Cil.SizeOf typ]))
                   **> comb **> z)
            
            | glob -> P.Copy.walk_fns.P.Walk.walk_glob glob
          end ;

          P.Walk.walk_func = begin (* Lower function bodies. *)
            fun (fun_var, fun_vars, fun_graph, fun_ppt) above ->
              above, begin fun comb z ->
                P.Build.func'' fun_ppt begin
                  fun_var, fun_vars, 
                  G.unfocus (lower_graph () fun_graph), (* lower the graph. *)
                  G.Ppt.from fun_ppt
                end **> comb **> z
              end
          end ; 
      } above program

  let of_program (program:P.t) : Program.comb =
    walk_program () program

end (* module Lower_meta *)


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Region_analysis  : sig

  (* Region_analysis : answers the question "how should the DPS
     transformation treat the return values coming out of this
     region?"
     
     In particular, we are interested both in regions that have global
     names (i.e., regions that exactly correspond to function bodies)
     as well as anonymous regions (regions nested within functions).
     
   [Return Modes]

     At the coarseness of this analysis, there are three potential
     _return modes_ (we call these rmodes), which we order (from
     "least" to "greatest") as: Direct, Indirect_stable and
     Indirect_changable.  

     Here are their brief descriptions:

     Name of rmode          Returns values ...
     -------------------------------------------------------------
     Direct              -- directly (i.e., via the stack)
     Indirect_stable     -- indirectly, through stable memory
     Indirect_changeable -- indirectly, through changeable memory

     The condition for when the Indirect_changeable rmode applies
     whenever, regardless of memoization, the returning region
     contains a read (in its dynamic scope); we use this condition to
     conservatively estimate if the return value may change upon
     re-execution.

     The Direct and Indirect_stable rmodes apply only when the return
     value will (upon re-execution) never change. The Direct rmode is
     used when the current local continuation (i.e., the one that
     reaches the return point in question) is not memoized;
     Indirect_stable applies otherwise (in these cases we store, in
     memory, the return values so that they can be recovered upon memo
     matches).

     [Context-Sensitivity]

     For named regions (i.e., functions), the question about
     permissible return value modes is always context-sensitive: it
     involves the region performing the tail-call and a (named) region
     being tail-called region (note that all calls to named regions
     are tail calls).

     For instance, if region R tail calls region S and either R or S
     contains a read, then both R and S must admit versions whose
     return values are returned indirectly, through changable
     memory.

     Since the analysis is context-sensitive, for each named region we
     give, as output, a _list_ of permissible rmodes (each of type
     dps_info).  When using these results during the DPS
     transformation, we select the appropriate dps_info instance,
     depending on context.

     [Special Case for Values Returned Directly]

     For dps_info instances where rmode = Direct, we preserve the
     original function name.  This simplifies a bunch of otherwise
     cumbersome technical issues: (1) it allows us to avoid renaming
     the entry points into the core program--they always return void
     (and thus can always be called with direct return); and (2) it
     naturally avoids renaming calls to foreign C functions (these
     necessary have direct return values---we cannot rewrite them to
     be otherwise).
  *)
  type rmode = 
    | Direct        (* values returned directly. *)
    | Indir_stable  (* values returned indirectly, through stable memory *)
    | Indir_change  (* values returned indirectly, through changeable memory *)

  val rmode_compare : rmode -> rmode -> int
  
  val string_of_rmode : rmode -> string

  type t      

  val of_program : Program.t -> t  
        
  (* the return mode of a cut (which specified by a program point) *)
  val rmode_of_cut : Zipcfg.ppt -> t -> rmode

  (* the return mode(s) of a function; there may be several since
     functions may require several instantiations (e.g., depending on how
     and where they are tail-called).  The list of rmodes always in
     decsending order. *)
  val rmodes_of_func : Zipcfg.var -> t -> rmode list
  
  (* Returns the "greatest" (i.e., "most stable") rmode permitted for
     the function *)
  val rmode_of_func : Zipcfg.var -> t -> rmode
    
  type dps_info = rmode * Zipcfg.var * Zipcfg.var list

  (* Similar to rmodes_of_func except that it includes extra info used
     for DPS conversion *)
  val dps_info_of_func : Zipcfg.var -> t -> dps_info list
      
  (* Print the results of the analysis (e.g., for debugging purposes) *)
  val print : out_channel -> t -> unit

  val check_dest_type : Zipcfg.ppt -> Cil.typ -> unit
end 
  = 
struct
  module G = Zipcfg
  module P = Program
  module PM = G.Ppt.Map
  module VM = Varinfo.Map
  module VS = Varinfo.Set

  let (@@>) = G.Walk.(@@>)
  let (@!>) = G.Walk.(@!>)

  let debug_flag = true

  exception Return_type_is_too_small of G.ppt * Cil.typ

  let check_dest_type (ppt:G.ppt) (dsttyp:Cil.typ) =
    if (Cil.bitsSizeOf dsttyp) < (Cil.bitsSizeOf Cil.voidPtrType) then begin
      Pretty.fprint stderr ~width:80 
        (Pretty.dprintf "Ceal Error %a: return-type `%s' is strictly smaller than a pointer (%d bits < %d bits)@!"
           Program_point.long_doc ppt 
           (string_of_type dsttyp) 
           (Cil.bitsSizeOf dsttyp) 
           (Cil.bitsSizeOf Cil.voidPtrType)
        ) ;
      raise (Return_type_is_too_small(ppt, dsttyp))
    end
    
  let ppt_of_func_var (var:G.var) (prog:P.t) : G.ppt =
    let _,funcs,_  = prog in
    let _,_,_,ppt  = P.Funcs.find var funcs in ppt

  (* (Region) Return mode : answers the question "how should the DPS
     transformation treat the return values coming out of this
     region?" *)
  type rmode = 
    | Direct        (* values returned directly. *)
    | Indir_stable  (* values returned indirectly, through stable memory *)
    | Indir_change  (* values returned indirectly, through changeable memory *)

  let string_of_rmode = function
    | Direct       -> "direct"
    | Indir_stable -> "indir_stable"
    | Indir_change -> "indir_change"

  (* - - -  Lattice - - - *)
  (* Define a lattice over the rmodes *)
  let rmode_compare a b = 
    let ord = function
      | Direct       -> 2 (* "greatest" = top *)
      | Indir_stable -> 1
      | Indir_change -> 0 (* "least" = bottom *)
    in
    (ord a) - (ord b)

  (* Map each function name to the set of calling modes used in the
     program.  An invariant: each calling mode is bounded above by the
     mode of the function body. *)
  module Func_modes = SetMap 
    (Set.Make(
       struct 
         type t = rmode 
         let compare = rmode_compare 
       end))(VM)

  let top = Direct
  let bot = Indir_change

  (* meet (greatest lower bound). *)
  let meet a b = match rmode_compare a b with
    | 0 (* a = b *) -> a
    | n when n < 0  -> a
    | n when n > 0  -> b
    | _             -> assert false
    
  (* - - - - Above / Below - - - - *)
  (* For now no data flows down the walk *)
  type above = unit

  (* From below: we build up an updated map for current iteration. *)  
  type below = {
    rmode : rmode ;        (* the region/return mode *)
    rmmap : rmode PM.t ;   (* the rmode of each region (by program point) *)
    funcs : Func_modes.t ; (* the calling mode(s) used for each function *)
    calls : VS.t ;         (* the functions called *)
  }
      
  let below_default = { 
    rmode = Direct ; 
    rmmap = PM.empty ;
    funcs = Func_modes.empty [] ;
    calls = VS.empty ;
  }

  let below_merge b1 b2 = 
    { rmode = meet b1.rmode b2.rmode ;            
      rmmap = PM.fold PM.add b1.rmmap b2.rmmap 
        (* rmmaps are disjoint ==> no meets needed *) ;
      funcs = Func_modes.merge b1.funcs b2.funcs ;            
      calls = VS.union b1.calls b2.calls }

  let below_region program ppt has_results = fun b ->
    
    let rmode = (* Should DPS-conversiont be selective? *)
      if ! Global_flags.dps_is_selective then b.rmode
      else 
        VS.fold begin fun var rmode ->
          if (P.func_var_is_foreign_c program var)
          then b.rmode 
          else 
            (* If DPS should not be selective, and if the region does not
               immediately call any conventional code, then we can always
               force a DPS conversion.   We do this as follows: *)
            Indir_change
        end 
          b.calls b.rmode 
      in

      let rmode = 
        (* No return results ==> 
           Every return result can be returned directly (trivially true.) *) 
      if has_results then rmode else Direct 
    in
    (* DEBUG/TEMP *) (*
    let _ = if debug_flag then ignore
      (log "  below_region: %a is %s\n" Program_point.doc pt 
         (string_of_rmode rmode))
    in *)
    { 
      (* clear the calls; since we only care about calls made
         immediately within the current region. *)
      calls = VS.empty ;      

      (* the rmode of a region that contains the current one;
         only Indir_change influences this outside region. *)
      rmode = begin match rmode with
        | Direct       -> Direct
        | Indir_stable -> Direct
        | Indir_change -> Indir_change
      end ;

      (* save the rmode of this region *)
      rmmap = PM.add ppt rmode b.rmmap ;

      (* for each function called (immediately) in this region, record the calling
         mode (i.e., the current region's mode); certain functions may
         require several versions after the dest-passing-style
         transformation.  *)
      funcs = VS.fold begin fun var funcs ->
        (* DEBUG/TEMP *) (*
        ignore (log "    calls %s as %s\n" 
                  var.Cil.vname (string_of_rmode rmode) ) ; *)
        Func_modes.extend var rmode funcs

      end b.calls b.funcs ;
    }

  (* - - - Utility combinators - - - *)
  let walk_nop = fun a -> a, fun b -> b

  let walk_nop_last = fun a -> below_default

  let walk_call resolve_func f = fun a -> 
    { below_default with
        rmode = resolve_func f ;
        calls = VS.singleton f }

  let walk_read = fun a -> a, fun b -> 
    { b with rmode = match b.rmode with
        | Direct       -> Indir_change
        | Indir_stable -> Indir_change
        | Indir_change -> Indir_change }
      
  let walk_memo = fun a -> a, fun b ->
    { b with rmode = match b.rmode with
        | Direct       -> Indir_stable
        | Indir_stable -> Indir_stable
        | Indir_change -> Indir_change }
    
  let walk_cut prog pt has_results = 
    fun a -> a, (below_region prog pt has_results)

  (* Walk over a graph *)
  let walk_graph 
      (program:P.t)
      (resolve_func:G.var -> rmode)
      (graph:G.t) 
      (a:above) : below 
      =
    let walk : (above,below) G.Walk.walk_fns -> above -> G.zt -> below = G.Walk.walk  in
    walk {            
      G.Walk.walk_first = (fun _ -> walk_nop) ;

      G.Walk.walk_nested = begin fun  _ a -> a, fun bs b -> 
        List.fold_left below_merge b bs
      end (* walk_nested *);
      
      G.Walk.walk_middle = begin function
        | G.Cut (label,m,pt) -> 
            let (_,_,vars,_) = G.info_of_label label graph in
            begin walk_cut program pt (vars <> [])
              @@> match m with 
                | G.Memo_yes _ -> walk_memo
                | G.Memo_no    -> walk_nop
            end              
        | G.Memo _                    -> walk_memo
        | G.Let_simp (_, G.Read _, _) -> walk_read
        | _                           -> walk_nop
      end (* walk_middle *);
      
      G.Walk.walk_last = begin function               
        (* Assume all calls are first-order *)
        | G.Call (_, Cil.Lval(Cil.Var f, Cil.NoOffset), _, _) -> 
            walk_call resolve_func f

        | G.Call _ -> assert false
        | _        -> walk_nop_last
      end (* walk_last *);
    } a (G.entry graph)

  (* Walk over a program *)
  let walk_program 
      (resolve_func:G.var -> rmode) 
      (program:P.t)
      (a:above) : below 
      =
    P.Walk.walk {
      
      P.Walk.walk_glob = begin fun _ -> walk_nop end ;
      
      P.Walk.walk_func = begin fun (v, _, graph, pt) a -> a, fun b ->         
        (* Walk the graph; 
           pick out the return mode and function call modes *)
        let graph_rmmap, graph_funcs =           
          let has_results = match v.Cil.vtype with
            | Cil.TFun(Cil.TVoid _, _, _, _) -> false
            | Cil.TFun(_,           _, _, _) -> true
            | _                              -> assert false
          in
          let b = 
            begin below_region program pt has_results
              **> walk_graph program resolve_func graph a end
          in
          (b.rmmap, 
           (* be sure to add the "default" mode *)
           Func_modes.extend v b.rmode b.funcs)
        in

        { b with 
            rmmap = PM.fold PM.add graph_rmmap b.rmmap ;
            funcs = Func_modes.merge graph_funcs b.funcs ;
        }
      end (* walk_func *) ; 
        
      P.Walk.walk_root = walk_graph program resolve_func ;

    } a program

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** Implementation of module interface *)

  type dps_info = rmode * Zipcfg.var * Zipcfg.var list
  
  type t = { rmode_of_cuts     : rmode PM.t ;
             dps_info_of_funcs : dps_info list VM.t }

  (* find the return mode of a cut *)
  let rmode_of_cut (ppt:G.ppt) (rmodes:t) =
    PM.find ppt rmodes.rmode_of_cuts

  (* find the dps info for a function *)
  let dps_info_of_func (var:G.var) (rmodes:t) =
    VM.find var rmodes.dps_info_of_funcs   

  (* find the return rmode(s) of a function *)
  let rmodes_of_func (var:G.var) (rmodes:t) =
    List.map begin fun (rmode, _, _) -> rmode 
    end (dps_info_of_func var rmodes)

  (* find the "greatest" rmode of a function *)
  let rmode_of_func (var:G.var) (rmodes:t) =
    match rmodes_of_func var rmodes with
      | rmode :: _ -> rmode
      | _          -> assert false

  (* Iterate over program until we reach a fix point *)
  let of_program (prog:Program.t) : t =
    
    let resolve_func_of_rmmap rmmap : G.var -> rmode = fun f ->
      (* Resolve functions to region_props using last mapping. *)
      try 
        let ppt = ppt_of_func_var f prog in
        if PM.mem ppt rmmap
        then PM.find ppt rmmap
        else top
      with 
        | Not_found -> top (* a foreign C function. *)
    in          
        
    let rec loop_until_fixpoint (rmmap_0 : rmode PM.t) =
                  
      (* Walk the program again *)
      let b = walk_program (resolve_func_of_rmmap rmmap_0) prog () in

      (* Have any properties changed? *)
      if not (PM.equal (fun a b -> rmode_compare a b = 0) b.rmmap rmmap_0)
      then
        (* Not at a fixpoint yet; so loop. *)
        loop_until_fixpoint b.rmmap 
      else
        (* The maps are equivalent ==> we are almost done;           
           lastly, we massage the results a bit by generating some new
           temporaries (used later by the DPS conversion) *)
        
        (* map each function to a list of dps_infos *)
        let dps_info_of_funcs : dps_info list VM.t = 
          let rf = resolve_func_of_rmmap b.rmmap in
          
          (* For each function, compute the dps infos *)
          Func_modes.M.fold begin fun var rmodes dps_info_of_funcs ->
            
            (* Put all the rmodes in descending order *)
            let rmodes    = Func_modes.S.add (rf var) rmodes in
            let rmodes    = List.rev (Func_modes.S.elements rmodes) in
            
            (* For each rmode, compute a dps_info (a triple) *)
            let dps_infos = List.map begin fun rmode ->
              
              (* For each rmode, compute the dps_info *)              
              if rmode = Direct then 
                (* We don't actually need a fresh variable here, do
                   we?  If we do -- then we need to fix up all
                   incoming calls from the meta program (since all
                   core entry points will be affected); otherwise,
                   since all core entry points return void, they can
                   return (void) directly (obviously). *)                
                (* (rmode, Temps.fresh_ver var, []) (* <-- Don't need a fresh variable ? *)*)
                (Direct, var, [])
                  
              else (* rmode requires a destination *)
                match var.Cil.vtype with                  
                  | Cil.TFun(t, Some formals, false, atts) -> 
                      let dstbse = Qual.Dest.type_of_basetype t in
                      let _      = check_dest_type (ppt_of_func_var var prog) dstbse in
                      let dsttyp = Cil.TPtr(dstbse, []) in
                      let dstvar = Temps.fresh_var dsttyp in
                      let dstfml = dstvar.Cil.vname, dsttyp, [] in
                      let var'_t = Cil.TFun(dsttyp, Some (formals @ [dstfml]), false, atts) in
                      let var'   = Temps.fresh_ver' var var'_t in
                      (rmode, var', [dstvar])                        
                  | _ -> assert false
                      
            end (* List.map *) rmodes
            in
            VM.add var dps_infos dps_info_of_funcs

          end (* Func_modes.M.fold *) b.funcs VM.empty
        in
        { rmode_of_cuts     = b.rmmap ;
          dps_info_of_funcs = dps_info_of_funcs }
    in 
    loop_until_fixpoint PM.empty

    
  let print out rmmodes =
    Printf.fprintf out "\n" ;
    Printf.fprintf out "Region_analysis: return modes for each function:\n" 
    ;
    VM.iter begin fun var rmodes ->
      Printf.fprintf stderr "  %20s : { " var.Cil.vname ;
      List.iter begin fun (rmode, _, _) ->
        Printf.fprintf stderr "%s, " (string_of_rmode rmode)
      end rmodes 
      ;
      Printf.fprintf stderr "} \n" 
    end rmmodes.dps_info_of_funcs
    ;
    Printf.fprintf stderr "\n" 
                                                        
end (* Region_analysis module *)


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Dest_passing = struct
  (* Do a walk over the graph:
     
     A stack (a list) of frames flows down the walk.  Each frame
     consists of a list of destinations, where each destination is
     represented as a free variable.
     
     To begin transforming a function, we augment it with a new
     argument (if the function returns a value).  If the return type
     was \tau, the new argument is of type \tau*.  The new return type
     is \tau*.
     
     To begin transforming a cut that returns to block L, we allocate
     destinations (inside of the cut) for each argument to L.  We
     transform the body of the cut using the new destinations.  We
     create a fresh block L' that accepts the destinations, reads from
     each of them, then passes the read values (and control) to L, the
     original return block.
  *)
  module Ppt = Program_point
  module G  = Zipcfg
  module P  = Program
  module W  = Zipcfg.Walk
  module B  = Zipcfg.Build
  module R  = Region_analysis

  let ( **> ) f x = f x

  let pure_of_var v = Cil.Lval(Cil.Var v, Cil.NoOffset)

  let rec pures_of_vars = function
    | []      -> []
    | v::vars -> (pure_of_var v)::(pures_of_vars vars)

  let lval_of_ptrvar var = (Cil.Mem (pure_of_var var), Cil.NoOffset)

  let raise_undefined_version (v:G.var) (rmode:R.rmode) =
    failwith (Printf.sprintf 
                "Region analysis should have (but did not) give us: %s for rmode %s"
                (string_of_var v) (R.string_of_rmode rmode))

  (* - - - - begin code that belongs here - - - - *)

  type frame = R.rmode * G.var list
  type stack = frame list      
      
  (* combinator for the boiler-plate code of a dps-converted cut.  *)
  let dps_cut (ppt:G.ppt) 
      (lab_ret:G.label)            (* return control to ret_lab    *)
      (memo:G.memo)                (* memo descriptor original cut *)
      (rmode:R.rmode)              (* return mode for the cut body *)
      (ds1:(Cil.typ * G.var) list) (* dests used within cut body   *)
      (ds2:(Cil.typ * G.var) list) (* dests used after  cut body returns *)
      : G.nodes
      =
    let lab_ret' = Temps.fresh_label (Ppt.loc ppt) in
    (* let dstvars  = List.map snd dests in *)
    let tmpvars  = List.map (fun (typ,_) -> Temps.fresh_var typ) ds2 in
    begin fun z ->
      B.let_block ppt lab_ret' (List.map snd ds2)
        begin
          (* Read/peek each destination into a temporary variable *)
          List.fold_right2 begin fun tmpvar dstvar nodes -> 
            begin fun z -> B.let_simp ppt tmpvar begin match rmode with
              | R.Direct       -> assert false
              | R.Indir_stable -> G.Peek (lval_of_ptrvar dstvar)
              | R.Indir_change -> G.Read (lval_of_ptrvar dstvar)
            end **> nodes **> z end
          end tmpvars (List.map snd ds2) begin
            (* Branch to the original return label (using tmpvars as args) *)
            B.branch ppt (G.Br(lab_ret, List.map pure_of_var tmpvars))
          end
        end           
      **> B.cut ppt lab_ret' memo        
      **> (List.fold_right begin fun (dsttyp, dstvar) z ->
             ( R.check_dest_type ppt dsttyp ;
               B.let_simp ppt dstvar (G.Alloc dsttyp) **> z )
           end ds1)
        (* Declare the return variables. *)
      **> B.rvars ppt (snd (List.split ds1))
      **> z
    end
      
  let walk_fns (program:P.t) (rmodes:R.t) (fun_ver:R.rmode -> G.var -> G.var) (graph:G.t) = { 
    G.Copy.walk_fns with
      W.walk_middle = begin fun middle stack -> match middle with

        | G.Memo (scope, ppt) -> begin match stack with
              (_, dstvars)::_ ->
                stack, begin fun z -> 
                  B.memo ppt scope
                  **> B.rvars ppt dstvars 
                  **> z end
                  
            | _ -> assert false
          end

        | G.Cut (label, memo, ppt) ->
            begin match R.rmode_of_cut ppt rmodes with
              | R.Direct -> (* no dps conversion necessary here. *)
                  (R.Direct, []) :: stack, 
                  begin function (* function over the zipper we get. *)                      
                      
                    (* Corner Case: Turn foreign C call into a G.Clib (See {!G.simp}) *)
                    (* See: Program_levels.Cut_conv_calls *)
                    (* TODO: What is the difference between "conventional" and "foreign_c"? *)
                    | ((h, G.Last(G.Call(G.Default,(Cil.Lval(Cil.Var fun_var, Cil.NoOffset)),pures,ppt))),_)

                        when ( P.func_var_is_foreign_c program fun_var ||
                                 Qual.var_is_foreign_c fun_var )  ->
                        
                        let temp_var, temp_pures = 
                          match Annot.arrow_type_unpack fun_var.Cil.vtype with
                            | None, _, [   ] -> Temps.wildcard_var, []
                            | None, _, [ t ] -> let v = Temps.fresh_var t in v, [pure_of_var v]
                            | _ -> failwith "Expected foreign C function to return 0 or 1 scalars."
                        in 
                        B.let_simp ppt temp_var (G.Clib(pure_of_var fun_var, pures)) 
                        **> B.branch ppt (G.Br (label, temp_pures))
                        **> G.emptyz

                    (* Usual case: nothing special to do. *)
                    | z -> B.cons_middle middle **> z
                  end
                    
              | rmode -> (* need a dps conversion for body. *)
                  let formals  = G.formals_of_label label graph in
                  let types    = List.map (fun v -> Qual.Dest.type_of_basetype v.Cil.vtype) formals in
                  let dstvar t = Temps.fresh_var (Annot.ptr_type_of_alloc_type t) in
                  let dests  _ = List.map (fun t -> (t, dstvar t)) types in
                  let ds1, ds2 = dests (), dests () in
                  (rmode, List.map snd ds1) :: stack,
                  dps_cut ppt label memo rmode ds1 ds2
            end (* match *)
              
        | _ -> stack, B.cons_middle middle
      end (* walk_middle *) ;
      
      W.walk_last = begin fun last stack -> 
        let rmode, dstvars = match stack with              
            (* An invariant states that the stack is non-empty. *)
          | (rmode, dstvars)::_ -> rmode, dstvars
          | []                  -> assert false
        in
        let last_nodes : G.nodes = match last with
            
          | G.Call (mode, (Cil.Lval (Cil.Var var, Cil.NoOffset)), args, ppt) -> 
              (* Get the appropriate version of this function *)
              let var' = fun_ver rmode var in
              B.call ppt mode (pure_of_var var') 
                (args @ (pures_of_vars dstvars))
                
          | G.Call (_, _, _, _) -> 
              (* We only support first-order programs right now. *)
              assert false
                
          | G.Return (retvals, ppt) -> begin match rmode with
              | R.Direct -> 
                  B.return ppt retvals 
                    
              | _ -> (* we use indirection: *) fun z ->
                  (List.fold_right2 begin fun dstvar retval z -> 
                     match rmode with R.Direct -> assert false 
                         
                       (* Indirect/stable uses pokes *)
                       | R.Indir_stable -> 
                           B.let_simp ppt Temps.wildcard_var 
                             (G.Poke ((lval_of_ptrvar dstvar), retval))
                           **> z
                             
                       (* Indirect/changeable uses writes *)
                       | R.Indir_change -> 
                           B.write ppt (lval_of_ptrvar dstvar) retval
                           **> z
                             
                   end dstvars retvals)
                  **> B.return ppt (List.map pure_of_var dstvars)
                  **> z
                    
            end (* return case. *)
              
          | last -> B.cons_last last
        in 
        (last_nodes G.emptyz)
      end (* walk_last *) ;
  }

  (* DPS-convert a program *)
  let of_program (rmodes:R.t) (program:P.t) : P.comb =

    (* Lookup the new function var for the given rmode *)
    let fun_vers = fun rmode var ->
      let _, var', _ = 
        try
          (* If this raises Not_found then it may mean that we are
             requesting an rmode that was not anticipated by the
             Region_analysis module. *)
          List.find 
            (fun (rmode', var', _) -> rmode' = rmode) 
            (R.dps_info_of_func var rmodes)
        with
          | Not_found -> (raise_undefined_version var rmode)
      in var'  
    in

    (* walk_fns for dps-converting the program *)
    P.Walk.walk {
      P.Copy.walk_fns with
        
        P.Walk.walk_glob = begin fun glob above -> 
          above, match glob with
            | P.Vard (var, ppt) -> begin try 
                (* Is this variable a function; does it have DPS info? *)
                let dps_infos = R.dps_info_of_func var rmodes in
                fun comb z ->
                  (* For each distinct dps version, build a new var decl. *)
                  List.fold_right begin fun (_, var', _) comb ->
                    fun z -> P.Build.vard ppt var' **> comb **> z
                  end dps_infos comb 
                  **> z
                    
              (* If no dps info for var, then we leave it alone *)
              with Not_found -> 
                fun comb z -> P.Build.vard ppt var **> comb **> z
              end
                
            (* We replace the original versions of functions. *)
            | P.Func _ -> (fun comb z -> comb z)
    
            (* All other globals are preserved *)
            | glob -> fun comb z -> 
                P.Build.glob glob 
                **> comb **> z
        
        end (* walk_glob *);

        P.Walk.walk_func = begin fun (var, args, graph, ppt) above ->
          above, fun comb frag ->

            (* For each distinct dps version, build a new func. *)
            List.fold_right begin fun (rmode, var', dstvars) comb ->
              
              let func = var', args @ dstvars, 
                G.unfocus begin 
                  W.walk 
                    (walk_fns program rmodes fun_vers graph)
                    [(rmode, dstvars)]
                    (G.entry graph)
                end, ppt in
              
              fun frag -> 
                P.Build.func ppt func 
                **> comb **> frag
                
            end (* List.fold_right *) 
              (R.dps_info_of_func var rmodes) comb

            **> frag
        end (* walk_func *)

    } () program

end (* Dest_passing module *)


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Compute the set of live variables for every program point in a graph *)
module Live_vars : sig 
  type t

  val of_graph  : Zipcfg.var list -> Zipcfg.t -> t
  val at_ppt    : Zipcfg.ppt -> t -> Zipcfg.var list
end 
  = 
struct
  module G = Zipcfg
  module W = Zipcfg.Walk
  module V = Varinfo.Set  (* sets of variables *)

  (* shorthands for variable-set operations *)
  let ( ++ ) a b = V.union  a b
  let ( -- ) a b = V.diff   a b
  let ( -  ) a b = V.remove b a
  
  let ( @@> ) = W.(@@>)

  module M = G.Ppt.Map    (* program point mappings *)
  module L = SetMap(V)(M) (* live vars at each prog point, note L.t = V.t M.t *)

  type above = { defs : V.t }
  
  type below = { 
    live : V.t ; (* <-- Invariant: live \subseteq defs. *)
    map  : V.t M.t }

  (* For the base cases *)
  let empty map = { live = V.empty ; map = map }
      
  (* Set the live variable set for a program point *)
  let set_live_at ppt live' : below -> below = fun b ->
    { live = live' ; map = M.add ppt live' b.map }

  (* For each CFG node, call this on the set of variables used there. *)
  let uses (ppt:G.ppt) (vs:V.t) : above -> above * (below -> below) =
    fun a -> a, (* we enforce live \subseteq defs here *)
      fun b -> set_live_at ppt (b.live ++ (V.inter a.defs vs)) b

  (* For each CFG node, call this on each variable defined there. *)
  let def (ppt:G.ppt) (v:G.var) : above -> above * (below -> below) =
    fun a -> { defs = V.add v a.defs }, 
      fun b -> set_live_at ppt (b.live - v) b

  (* For CFG nodes that do not contribute to the analysis *)
  let nop ppt = fun a -> a, 
    fun b -> set_live_at ppt b.live b
      
  (* For CFG nodes that have no program point *)
  let nop_no_save = fun a -> a, fun b -> b

  let walk_fns (g:G.t) (map_0: L.t) : (above, below) W.walk_fns = 

    (* - - - used variables - - - *)
    
    (* In these cases the used variables are just the free variables: *)
    let uv_of_lval  = G.Free_vars.fv_of_lval  in
    let uv_of_pure  = G.Free_vars.fv_of_pure  in
    let uv_of_pures = G.Free_vars.fv_of_pures in
    let uv_of_simp  = G.Free_vars.fv_of_simp  in
    
    let uv_of_label label = 
      L.get (G.ppt_of_label label g) map_0
    in

    let uv_of_scope = function
      | G.Scope_same        -> V.empty
      | G.Scope_change pure -> uv_of_pure pure 
    in

(*
    let uv_of_memo  = function
      | G.Memo_yes scope    -> uv_of_scope scope
      | G.Memo_no           -> V.empty
    in
*)
    
    (* we use both [map_0] and [g] to compute used vars of branches: *)
    let uv_of_branch (G.Br (label, pures)) =
      (uv_of_label label) ++ (uv_of_pures pures)
    in

    (* - - - walk the graph - - - *)
    {     
      W.walk_first = begin function
        | G.Entry -> nop_no_save
        | G.Label(_, _, vars, ppt) -> 
            begin G.Walk.fold (def ppt) vars
              @@> uses ppt V.empty 
            end
      end ;

      W.walk_middle = begin function
        | G.Let_block (_, ppt) -> nop ppt
        | G.Update ppt         -> nop ppt
        | G.Memo (scope, ppt)  -> uses ppt (uv_of_scope scope)
            
        | G.Rvars (rvars, ppt) -> uses ppt 
            (uv_of_pures (List.map Abbrev.pure_of_var rvars))

        | G.Write (lval, pure, ppt) -> 
            uses ppt (uv_of_lval lval ++ uv_of_pure pure)

        | G.Let_simp (var, simp, ppt) ->
            begin def  ppt var
              @@> uses ppt (uv_of_simp simp) 
            end
        
        | G.Cut (label, memo, ppt) -> 
            begin uses ppt (uv_of_label label)
              @@> begin match memo with 
                | G.Memo_no               -> nop_no_save
                | G.Memo_yes (ppt, scope) -> uses ppt (uv_of_scope scope)
              end
            end
      end ;
    
      W.walk_last = begin
        let uses' ppt vs a = 
          (snd (uses ppt vs a)) (empty map_0) 
        in
        function
          | G.Exit -> (fun _ -> empty map_0)
            
          | G.Cond (pure, br1, br2, ppt) ->
              uses' ppt (uv_of_pure pure 
                       ++ uv_of_branch br1 
                       ++ uv_of_branch br2)
                
          | G.Branch (br, ppt) ->
              uses' ppt (uv_of_branch br)
                
          | G.Call (_, pure, pures, ppt) -> 
              uses' ppt (uv_of_pure pure 
                       ++ uv_of_pures pures)
                
          | G.Return (pures, ppt) -> 
              uses' ppt (uv_of_pures pures)
        end ;

      W.walk_nested = begin fun _ above -> above,
        fun b bs -> List.fold_right begin fun b1 b2 ->
          { live = b1.live ++ b2.live    ;
            map  = L.merge b1.map b2.map }
        end b bs
      end ;
    }
      
  type t = L.t

  let of_graph (defs0:G.var list) (graph:G.t) : t =
    let rec loop_until_fixpoint (map_0:L.t) : L.t =
      let map = ( W.walk (walk_fns graph map_0)
                    { defs = Varinfo.set_from_list defs0 }
                    (G.entry graph) ).map 
      in
      if L.equal map map_0 then map_0
      else loop_until_fixpoint map
    in loop_until_fixpoint M.empty

  let of_graph defs0 graph =
    let map = of_graph defs0 graph in
    let _ = if ! Global_flags.debug_live_vars then 
      M.iter begin fun pt vars ->
        ignore (log "%a : {%a}\n"
                  Program_point.doc pt
                  G.Print.pr_vars_doc (V.elements vars))
      end map
    in map
    
  let at_ppt (ppt:G.ppt) (map:t) : G.var list = 
    L.S.elements (L.get ppt map)

end (* Live_vars module *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Predecessors : sig
  type t        

  val empty    : t
  val entry    : Zipcfg.ppt
  val of_graph : bool -> Zipcfg.t -> t  
  val points   : t -> Zipcfg.ppt list
  val preds_of : t -> Zipcfg.ppt -> Zipcfg.ppt list 
  val succs_of : t -> Zipcfg.ppt -> Zipcfg.ppt list 
end 
  = 
struct  
  (* predecessor mapping -- from program points to sets of program points *)
  (* We use this mapping as our "augmented graph" representation---
     the representation of the graph that contains the extra entry
     edges to read nodes.  This works out to sufficiently represent
     the graph for the purposes of computing dominator information
     (i.e., we need only know the predecessor relation for the graph
     nodes to compute the dominator relation, and associated
     relations, like immediate dominance, etc.). *)
  module G = Zipcfg
  module W = Zipcfg.Walk
  module P = SetMap(G.Ppt.Set)(G.Ppt.Map)
    
  let empty : P.t = P.empty []

  (* A (unique) synthetic point representing inter-procedural control
     during change propgation (when read nodes are affected) *)
  let entry = G.Ppt.root ~props:[G.Property.Root_entry] Cil.locUnknown None

  (* Add the edge ppt1 ---> ppt2, (meaning that ppt1 is a predecessor of ppt2) *)
  let add_edge    ppt1    ppt2    preds = P.extend ppt2 ppt1 preds
  let add_edge_op ppt1_op ppt2_op preds = match (ppt1_op, ppt2_op) with
    | Some ppt1, Some ppt2 -> add_edge ppt1 ppt2 preds
    | _                    -> preds
            
  (* BUG-FIX-- Feb 17.2011.  *)
  (* It's been partially verified: when compiling existing benchmarks,
     everything still compiles and the resulting binaries pass
     regression tests.  *)
  let walk_fns (include_entry_to_read_edges:bool) (graph:G.t) = {
    W.walk_first = begin fun f _ ->
      let noop preds = preds in
      match f with
        | G.Entry            -> Some entry, noop
        | G.Label (_,_,_,pp) -> Some pp,    noop
    end ;
    W.walk_middle = begin fun m pred ->
      Some (G.ppt_of_middle m),
      begin fun preds -> 
        let preds      = add_edge_op pred (Some (G.ppt_of_middle m)) preds in
        let update ppt = 
          (* handle the case where the simp has an implicit update point. *)
          if include_entry_to_read_edges 
          then add_edge entry ppt preds 
          else                    preds
        in
        match m with 
          | G.Update ppt                  -> update ppt
          | G.Let_simp (_, G.Read _, ppt) -> update ppt
          | G.Let_simp (_, G.Ffi (_,op,_), ppt) when op.Ffi.flags.Ffi.wakeup -> update ppt
          | G.Cut (label, _, ppt) -> add_edge ppt (G.ppt_of_label label graph) preds
          | _ -> preds
      end
    end ;
    
    W.walk_last = begin fun last pred ->
      let ppt = G.ppt_of_last last in
      
      (* add an edge between this node's pred and itself *)
      let preds = add_edge_op pred ppt empty in
      
      (* add edges from this node to a branch label *)
      let walk_branch br preds = match br with
        | G.Br (label, _) -> add_edge_op ppt 
            (Some (G.ppt_of_label label graph)) preds
      in
      match last with
        | G.Exit                    -> preds (* NOTE: does not contribute as a predecessor *)
        | G.Cond   (_, br1, br2, _) -> walk_branch br1 (walk_branch br2 preds)
        | G.Branch (br, _)          -> walk_branch br preds 
        | G.Call   _                -> preds (* NOTE: does not contribute as a predecessor *)
        | G.Return _                -> preds (* NOTE: does not contribute as a predecessor *)
    end ;

    W.walk_nested = begin fun labels _ ->
      None, fun nested_preds preds ->
        List.fold_left begin fun a b ->
          P.merge a b end preds nested_preds
    end ;
  }

  type t = P.t

  let of_graph (include_entry_to_read_edges:bool)  (graph:G.t) : P.t =
    W.walk (walk_fns include_entry_to_read_edges graph) (Some entry) (G.entry graph)

  let points (preds:t) : G.ppt list =
    P.M.fold (fun ppt _ points -> ppt :: points) preds []

  let preds_of (preds:t) (ppt:G.ppt) : G.ppt list =
    G.Ppt.Set.elements (P.M.find ppt preds)

  let succs_of (preds:t) : G.ppt -> G.ppt list =
    (* To answer some queries, we need to have the inverse of the
       predecessor mapping (i.e., we need to know what successors of
       each program point) *)
    let succ_map = 
      List.fold_left begin fun map ppt_2 ->
        List.fold_left begin fun map ppt_1 -> 
          P.extend ppt_1 ppt_2 map (* add the edge pt_1 --> pt_2 *)
        end map (preds_of preds ppt_2)
      end P.M.empty (points preds)
    in 
    (fun ppt -> G.Ppt.Set.elements (P.M.find ppt succ_map))
    
end (* Predecessors module *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Dom_relation = struct
  module type NODE = sig
    type t
    val compare : t -> t -> int
    val preds   : t -> t list
    val name    : t -> string
    val ename   : t -> string
  end  

  module Make(N:NODE) = struct
    
    (* Sets of nodes *)
    module S = Set.Make(N)
      
    (* Map nodes to other things *)
    module M = Map.Make(N)

    (* Dominator Mapping -- maps nodes to sets of dominating nodes. *)
    module D = SetMap(S)(M)
      
    (* Immediate dominator map -- mapes nodes to nodes *)
    type idom_map = N.t M.t

    (* Dominator tree *)
    type dom_tree = Dom_tree of N.t * dom_tree list
      
    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)     
    (* Compute Dom(.) function for a graph
       -- where Dom(.) has type D.t
       -- uses quadratic algorithm, (Fig 7.14 from Muchnick 1997) *)
    let dominators (nodes:N.t list) (root:N.t) : (D.t) =
      let nodes         = List.fold_right S.add (root::nodes) S.empty in
      let nonroot_nodes = S.remove root nodes in
      
      (* Initialize dom:
         1, dom(root) = {root}. 
         2. For all non-root nodes $n$, $dom(n)$ = nodes.
      *)  
      let dom = ref (D.empty []) in
      let _ = dom := (D.extend root root !dom) in
      let _ = S.iter (fun node -> dom := (D.M.add node nodes !dom))
        nonroot_nodes 
      in                                                
      let change = ref false in

      (* Visit a node and estimate its dominators based on the
         dominators of its predecessors *)
      let visit_node node =
        let node_dom = (D.get node !dom) in
        let node_dom' = (List.fold_left 
                           (fun node_dom' pred -> 
                              let pred_dom = D.get pred !dom in
                              (S.inter node_dom' pred_dom))
                           node_dom (N.preds node))
        in
        let node_dom' = S.add node node_dom' in
        if not (S.equal node_dom' node_dom) then begin
          change := true ; 
          dom := D.M.add node node_dom' !dom ;
        end
      in
      (* Loop until no change *)
      let rec loop _ =  
        begin
          change := false ;
          (S.iter visit_node nonroot_nodes) ;
          if !change then loop () else ()
        end
      in 
      (loop () ; !dom)

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (* Compute immediate dominators 
       -- uses quadratic algorithm, (Fig 7.15 from Muchnick 1997) *)
    let immediate_dominators (nodes:N.t list) (root:N.t) (dom:D.t) : idom_map =
      let nodes         = List.fold_right S.add (root::nodes) S.empty in
      let nonroot_nodes = S.remove root nodes in
      
      (* Initially: tmp_dom(node) = (dom(node) - {node}). *)
      let tmp_dom = ref (D.empty []) in
      let _ = (S.iter 
                 (fun node -> 
                    let node_dom = D.get node dom in
                    let node_dom' = S.remove node node_dom in
                    tmp_dom := (D.M.add node node_dom' !tmp_dom))
                 nodes)
      in
      (* Visit a node and two of its distict dominators *)
      let visit_node node node_dom1 node_dom2 =
        if S.mem node_dom2 (D.get node_dom1 !tmp_dom) then
          tmp_dom := (D.remove node node_dom2 !tmp_dom)
      in  
      begin 
        let iter nodes fn = S.iter fn nodes in
        (* Consider all nodes and all choices of two distict dominators *)
        (iter nonroot_nodes 
           (fun node -> iter (D.get node !tmp_dom)
              (fun node_dom1 -> iter (D.get node !tmp_dom)
                 (fun node_dom2 -> if not (node_dom1 == node_dom2) then
                    (visit_node node node_dom1 node_dom2))))) ;    

        (* Assert that all nodes now have a unique immediate dominator
           and create and return such a mapping for the nodes *)
        (S.fold 
           (fun node (idoms : idom_map) -> 
              let node_idoms = D.get node !tmp_dom in
              (* No immediate dominator ==> unreachable code *)
              if S.is_empty node_idoms then 
                idoms
              else
                (* Some immediate dominator ==> it is unique. *)
                let node_idom1 = S.min_elt node_idoms in
                let node_idom2 = S.max_elt node_idoms in
                if node_idom1 == node_idom2 then 
                  (M.add node node_idom1 idoms) 
                else 
                  assert false)
           nonroot_nodes M.empty)
      end

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (* Compuate a dominator tree given a root node and a immediate
       dominator mapping *)
    let dominator_tree (root:N.t) (idom_map:idom_map) : D.t * dom_tree = 
      let inverse_map : D.t = 
        M.fold begin fun node idom inverse_map ->
          D.extend idom node inverse_map
        end idom_map (D.empty []) 
      in
      let rec build (node:N.t) =
        let children = S.elements (D.get node inverse_map) in
        Dom_tree (node, List.map build children)
      in inverse_map, build root
    
    let dump (tree:dom_tree option) out =
      Printf.fprintf out "digraph {\n" ;
      let rec dump_rec (parent:N.t option) = function
        | Dom_tree (n, trees) ->
            Printf.fprintf out "node_%s [label=\"%s\"];\n" 
              (N.ename n) (N.ename n)
            ;
            begin match parent with
              | None        -> ()
              | Some parent -> Printf.fprintf out "node_%s -> node_%s;\n"
                  (N.ename parent) (N.ename n)
            end 
            ;
            List.iter (dump_rec (Some n)) trees
      in 
      begin match tree with 
        | None   -> ()
        | Some t -> dump_rec None t
      end ;
      Printf.fprintf out "}"

  end (* end Make module *)
end (* end Dom_relation module *)

(* Instansiates the Dom_relation functor above in a commonly-used way. *)
module Dom_summary = struct
  
  module G     = Zipcfg
  module Preds = Predecessors
      
  type node = G.ppt * Preds.t

  (* Dominator relation for these "nodes". *)
  module Dom  = Dom_relation.Make
    (struct 
       type t          = node
       let compare a b = G.Ppt.compare (fst a) (fst b)
       let preds a     = List.map (fun pred -> (pred, snd a)) (Preds.preds_of (snd a) (fst a))
       let name a      = Pretty.sprint ~width:80 (G.Ppt.doc () (fst a))
       let ename a     = Pretty.sprint ~width:80 (G.Ppt.edoc () (fst a))
     end)    

  include Dom
    
  type summary = {
    pts      : G.ppt list ;
    doms     : Dom.D.t ;
    imm_doms : Dom.idom_map ;
    dom_tree : Dom.dom_tree ;
    dom_kids : Dom.D.t ; (* dom_kids: like dom mapping above, except here the
                            image only includes those blocks that we 
                            immediately dominate. *)
  }

  let of_graph 
      (include_entry_to_read_edges:bool) 
      (out_op:out_channel option) 
      (graph:G.t) 
      : summary 
      = 
    let preds : Preds.t = Preds.of_graph include_entry_to_read_edges graph in    
    let nodes : node list = List.fold_left begin 
      fun nodes pp -> (pp, preds) :: nodes end [] 
      (Preds.points preds)
    in
    let root : node = (Preds.entry, preds) in    
    let doms        = Dom.dominators nodes root in
    let imm_doms    = Dom.immediate_dominators nodes root doms in
    let dom_kids, dom_tree = Dom.dominator_tree root imm_doms 
    in
    let _ = match out_op with
      | Some out -> Dom.dump (Some dom_tree) out
      | None     -> ()
    in
    { pts      = List.map fst nodes ;
      doms     = doms ;
      imm_doms = imm_doms ;
      dom_tree = dom_tree ;
      dom_kids = dom_kids }

  let dom_kids : summary -> G.ppt -> G.ppt list =
    (fun summary pt -> List.map fst 
       (Dom.D.S.elements (Dom.D.get (pt,Preds.empty) summary.dom_kids)))

  let imm_dom : summary -> G.ppt -> G.ppt =
    (fun summary pt -> fst (D.M.find (pt,Preds.empty) summary.imm_doms))

end



(* Trace node descriptors --- "descriptors" for short.
   
   Each descriptor corresponds to a chunk CEAL code that is traced
   as one inseparable unit.  Each time the chunk is traced, the RT
   generates a new trace node _instance_ that carries a pointer to
   the corresponding descriptor.  Descriptors carry information that
   is (1) known statically and (2) shared across trace node
   instances, hence, in the target code we represent these
   descriptors as static objects.
   
   Each descriptor contains:
   
   -- a size (in bytes) for its instances.  This includes the space
   for the pointer that points back to the descriptor.
   
   -- a set of flags indicating what optional fields are present in
   the instances.  Optional fields include an (1) end time and (2) a
   memo table.  [TODO: In the future we should also support making
   the start time optional, but for the time being this would be too
   complicated to implement (and it would be useful only for trace
   nodes that exclusively contain allocations, perhaps an uncommon
   case?)]
   
   -- pointers to the undo / redo functions.
*)
module Trnode_descriptor = struct
        
  (* Flags determine two things:
     
     (1) which fields are present in the header of each instance 
     
     (2) when the start time is assigned to each instance (either
     immediately, or after a memo miss, in the case it
     has_memo_entry)
  *)
  type flags = {
    has_start_time : bool ; (* for now, always true *)
    has_memo_entry : bool ; (* ==> saves_scope *)
    has_unboxed    : bool ; (* ==> reclamation must be postponed *)
    saves_scope    : bool ; (* ==> need a field to save the scope *)
    has_end_time   : bool ; (* ==> need a field to store end time *)
  }

  (* A descriptor consists of some flags and some shared functions *)
  type 'a t = {
    ppt   : Zipcfg.ppt ;
    size  : Cil.exp  ;
    flags : flags    ;
    redo  : Cil.exp  ;
    undo  : Cil.exp  ;
  }
  
  let ppt_none = Program_point.none

  let check_flags flags = 
    (* Check for valid configurations of the descriptor flags. We
       want to ensure that if a flag is present, we always know a
       priori how to find it within the trace node structure
       (independently of whether the other flags are present or
       not).  This means that only certain configurations are
       permitted. *)
    let okeydoke = () in
    match 
      flags.has_start_time,
      flags.has_memo_entry,
      flags.has_unboxed,
      flags.saves_scope, 
      flags.has_end_time
    with (* Valid cases: *)
      | false, false, _, false, false -> okeydoke
      | true,  false, _, false, false -> okeydoke 
      | true,  _,     _, true,  false -> okeydoke
      | true,  _,     _, true,  true  -> okeydoke
          
      (* All other cases are invalid: *)
      (* TEMP / DEBUG *)
      | a, b, c, d, e -> 
          Printf.eprintf "Invalid flags: %s=%B %s=%B %s=%B %s=%B %s=%B\n" 
            "has_start_time" a 
            "has_memo_entry" b 
            "has_unboxed"    c
            "saves_scope"    d 
            "has_end_time"   e 
          ; assert false
            
  let payload_of_flags : flags -> Cil.typ list = fun flags -> 
    let ptr_typ t = 
      Cil.TPtr(t, []) 
    in      
    let _ = 
      check_flags flags 
    in
    List.map fst **>
      List.filter snd
      [ (* BEWARE!: the order is very sensitve---
           it must match the order used in runtime.h 
           (for trace headers of instances).
           If this order is not preserved, an mysterious error may occur.
        *)
        (* start times: In the future we should support making the
           start time optional, but for the time being this would be
           too complicated to implement (and it would be useful only
           for trace nodes that exclusively contain allocations,
           which is perhaps an uncommon case?)  *)
        ptr_typ (Low.type_of_type_spec ppt_none Low.T_desc), true ;
        Low.type_of_type_spec ppt_none Low.T_time,           true (* TEMP: always present *);
        Low.type_of_type_spec ppt_none Low.T_scope,          flags.saves_scope;
        ptr_typ (Low.type_of_type_spec ppt_none Low.T_time), flags.has_end_time;
      ]
      
  (* Returns a CIL global and a CIL expression.
     
     The expresion returned is a pointer to a descriptor structure;
     the global returned declares and initializes this descriptor
     structure (statically). 

     After the global is declared, the descriptor pointer can then
     be used to construct new trace nodes (using the Low.Trace_node
     operation, above)
  *)
  let to_cil : 'a t -> Cil.global list * Cil.exp = fun desc ->
    let desc_var_sym : string  = Temps.fresh_sym () in
    let desc_typ     : Cil.typ = Low.type_of_type_spec ppt_none Low.T_desc in
    let stats_typ    : Cil.typ = Low.type_of_type_spec ppt_none Low.T_desc_stats in
    let stats1_var   : Cil.varinfo = Temps.fresh_var stats_typ in
    let stats2_var   : Cil.varinfo = Temps.fresh_var stats_typ in
    let init_exps    : Cil.exp list = 
      let exp_of_bool = 
        fun b -> if b then Cil.one else Cil.zero 
      in
      let exp_of_string s = 
        Cil.Const (Cil.CStr s) 
      in          
      let exp_of_int i = 
        Cil.integer i
      in
      let null = 
        Cil.mkCast ~e:Cil.zero ~newt:Cil.voidPtrType 
      in
      [ (* BEWARE!: the order is very sensitve---
           it must match the order used in runtime.h 
           (for trace node descriptors).
           If this order is not preserved, a mysterious error may occur.
        *)
        (* start times: we always include a start_time in the header
           of trace node instances, even if we need not.  This is
           due to implementation difficulties in realizing the
           start-time free cases. (see similar note above) *)
        desc.size ;
        exp_of_bool (desc.flags.has_start_time || true) (* TEMP, see above *);
        exp_of_bool desc.flags.has_memo_entry ;
        exp_of_bool desc.flags.has_unboxed ;
        exp_of_bool desc.flags.saves_scope ;
        exp_of_bool desc.flags.has_end_time ;
        desc.redo ;
        desc.undo ;
        exp_of_string desc_var_sym ;
        exp_of_string (Zipcfg.Ppt.file desc.ppt) ;
        exp_of_int    (Zipcfg.Ppt.line desc.ppt) ;
        exp_of_string (Printf.sprintf "%s (%s)" 
                         (Zipcfg.Ppt.long_string desc.ppt) 
                         desc_var_sym) ;
        null ;            
        Cil.AddrOf (Cil.Var stats1_var, Cil.NoOffset) ;
        Cil.AddrOf (Cil.Var stats2_var, Cil.NoOffset) ;
      ]
    in
    let init : Cil.init = 
      match Cil.unrollType desc_typ with
        | (Cil.TComp (ci, _)) as typ ->
            Cil.CompoundInit begin typ, 
              List.map2 begin fun fieldinfo init_exp ->
                Cil.Field(fieldinfo, Cil.NoOffset), 
                (Cil.SingleInit init_exp)
              end ci.Cil.cfields init_exps
            end
              
        | _ -> assert false
    in
    let desc_var = Temps.fresh_var' desc_var_sym desc_typ in
    [ Cil.GVar (stats1_var, {Cil.init = None}, Cil.locUnknown) ;
      Cil.GVar (stats2_var, {Cil.init = None}, Cil.locUnknown) ;
      Cil.GVar (desc_var, {Cil.init = Some init}, Cil.locUnknown) ;
    ],
    Cil.AddrOf (Cil.Var desc_var, Cil.NoOffset)
      
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module Lower_core = struct
  module G  = Zipcfg
  module W  = Zipcfg.Walk
  module B  = Zipcfg.Build
  module P  = Program    
  module V  = Varinfo.Set
  module L  = Low
  module R  = Region_analysis

  let ( **> ) f x = f x
  let ( @@> ) f g = W.(@@>) f g
  let ( @!> ) f g = W.(@!>) f g

  (* TODO: Put these into a Common module *)
  let offs_of_fld  fld  = Cil.Field (fld, Cil.NoOffset)
  let lval_of_var  v    = Cil.Var v, Cil.NoOffset
  let pure_of_var  v    = Cil.Lval (lval_of_var v)
  let pure_of_lval lval = Cil.Lval lval

  let lval_of_pure = function 
    | Cil.Lval lval -> lval
    | _             -> invalid_arg "lval_of_pure"

  let var_of_pure = function
    | Cil.Lval (Cil.Var v, Cil.NoOffset) -> v
    | _                                  -> invalid_arg "var_of_pure"

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Modes, Trace Nodes & Phases } *)

  (* Modes:

     Each point of the input program falls into one of three "modes",
     depending on whether it corresponds to code that will be:
     
     1) Traced and later undone, but not redone (Traced Pre_redo).
     
     2) Traced, redone and undone (Traced Post_redo).

     3) Neither redone nor undone (Untraced).

     Trace Nodes & Phases:

     A "trace node" consists of a straight-line sequence of CFG nodes,
     each of which has a Traced mode--either Pre_redo or
     Post_redo--which we refer to as it's "phase".  The trace node
     consists of at most one phase transition, which is always from
     the Pre_redo phase to the Post_redo phase.

     If the Post_redo phase exists for a trace node, then we call the
     first program point in this phase the "redo point" (we sometimes
     say that the program "wakes up" at this point to respond to
     changes).
  *)

  type 'a mode =
    | Untraced
    | Traced of phase * 'a 
        
  and phase =   (* which "phase" of the trace node are we at? *)
    | Pre_redo  (* code that is traced but never redone  *)
    | Post_redo (* code that is traced and can be redone *)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Data moving down and up walk -- [above] and [below], respectively} *)

  module Varmap = Varinfo.Map
        
  type need_apart = Apart_yes | Apart_no

  type above = {
    mode       : G.var list mode ; (* Trace mode,  Untraced or Traced *)
    need_apart : need_apart ;      (* Are we generating apart code? *)
    rmode      : R.rmode ;         (* Return mode, Direct or Indirect *)
  }
  
  type below = {
    data  : payload list -> payload list ;
    flags : flags ;
    decls : Program.comb ;
    funcs : Program.comb ;

    (* We build (up to) four graphs simultaneously *)
    whole : recov -> G.nodes ;
    apart : recov -> G.nodes ;
    redo  : recov -> G.nodes ;
    undo  : recov -> G.nodes ;
    
    (* For generating memoized returns: *)
    rvars : G.var list option ; (* return variables *)
    rlets : (recov -> G.nodes) Varmap.t; (* let-bindings for memoized return variables *)
  }
      
  (* space in the trace node for (unboxed) satellite data. *)
  and payload = 
    | Var     of fdesc  (* Space for saving a live program variable *)
    | Handle  of fdesc  (* Space for a RDS invoke/revoke handle. *)

  (* we leave holes in trace node code that we fill once all code
     (with holes) is complete. *)
  and pure_hole = 
    | Null_hole
    | Trnd0 | Trnd0_payload of payload
    | Trnd1 | Trnd1_payload of payload

  and fdesc = string * Cil.typ    (* descriptors for payload fields *)
  and alias = string * string     (* alias name, resolved name *)
  and recov = pure_hole -> G.pure (* recover pures to fill holes *)
        
  (* We use these flags to determine the trace node descriptor flags *)
  and flags = {
    flag_memo    : bool ; (* begins with a memo (true), or not (false) *)
    flag_unboxed : bool ; (* contains 1 or more unboxed allocations *)
    flag_read    : bool ; (* contains 1 or more reads *)
    flag_write   : bool ; (* contains 1 or more writes *)
    flag_return  : bool ; (* redo code ends with a return. *)
  }

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Misc Utilities } *)

  let fdesc (typ:Cil.typ) = (Temps.fresh_sym (), typ)

  let recov_none : recov = fun _ -> assert false    
    
  let default_flags : flags =
    { flag_memo    = false ;
      flag_unboxed = false ;
      flag_read    = false ;
      flag_write   = false ;
      flag_return  = false }

  let empty_below =
    { data  = (fun p   -> p) ;
      flags = default_flags  ;
      decls = (fun z   -> z) ;
      funcs = (fun z   -> z) ;
      whole = (fun _ z -> z) ;
      apart = (fun _ z -> z) ;
      redo  = (fun _ z -> z) ;
      undo  = (fun _ z -> z) ;
      rvars = None ;
      rlets = Varmap.empty ;
    }

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 'Allotments' for fields in the (temporary) trace node }  *)

  let allot_handle (ppt:G.ppt) (h:L.type_spec) : pure_hole * payload = 
    let p = (Handle (fdesc (L.type_of_type_spec ppt h))) in 
    Trnd0_payload p, p

  let allot_var (v:G.var) : pure_hole * payload =
    let p = (Var (v.Cil.vname, v.Cil.vtype)) in 
    Trnd0_payload p, p

  let allot_space (t:Cil.typ) : pure_hole * payload =
    let p = (Handle (fdesc t)) in Trnd0_payload p, p

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Lift label arguments } *)

  (* Augment a list of items (each corresponding to a variable
     argument to the given labeled block), with a second list of
     items (each corresponding to a live variable at the given block
     label--a free variable required by the block, but not included
     in block's formal arguments).  This routine is used to "lift"
     labels and the branches that target them when these edges
     become "cross-unit".  *)
  let lift_label_gen 
      (* customizers: *) (graph:G.t) (live:G.ppt -> G.var list) (mapf : G.var -> 'b)
      (* two "ordinary" arguments *) (label:G.label) (orig : 'a list) : 'b list =
    let origv  = let (_,_,vars,_) = G.info_of_label label graph in vars in
    let orig'  = List.map2 (fun v o -> (v, o)) origv orig in
    let extra  = List.map  (fun v -> (v, mapf v)) (live (G.ppt_of_label label graph)) in
    (* Should we reorder them? *)
    List.map snd (orig' @ extra)

    
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Generate walk functions for lowering graph nodes} *)

  let null   : G.pure = Cil.mkCast ~e:Cil.zero ~newt:Cil.voidPtrType 
  
  let empty  : recov -> G.nodes = fun _ z -> z 
  
  let wc_var : G.var = Temps.wildcard_var 
  
  module Strmap = Map.Make(String)

  let walk_fns      
      (live_vars_at  : G.ppt -> G.var list)
      (func_of_label : G.label -> (G.var * G.var list * G.nodes) option)
      (graph         : G.t) 
      (rmodes        : R.t)
      (rtypes        : G.ppt -> Cil.typ list)
      : (above, below) W.walk_fns =
    
    let lift_label_pures = lift_label_gen graph live_vars_at pure_of_var in

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Begin a trace node} *)
    let trnode_begin (ppt:G.ppt) : (above,below) W.walk_comb =
      
      let doit (below:below) =
        
        let desc_flags : Trnode_descriptor.flags = 
          let f = below.flags in {
            Trnode_descriptor.has_start_time = 
              f.flag_memo || f.flag_read || f.flag_write ;
            
            Trnode_descriptor.has_memo_entry = f.flag_memo ;
            
            Trnode_descriptor.has_unboxed = f.flag_unboxed ;

            Trnode_descriptor.saves_scope = 
              f.flag_memo || (f.flag_read && (not f.flag_return)) ;
            
            Trnode_descriptor.has_end_time =
              (f.flag_read || f.flag_memo) && (not f.flag_return) ;
          }
        in

        (* header is determined by descriptor flags *)
        let header : payload list = List.map 
          (fun (typ : Cil.typ) -> Handle (fdesc typ))
          (Trnode_descriptor.payload_of_flags desc_flags) 
        in        

        (* payload is determined by below.data *)
        let payload : payload list = below.data [] in
        
        (* Generate a custom composite type for the trace node. *)
        (* TODO: What about end times and/or memo table pointers? *)
        let ci : Cil.compinfo = 
          let comp_name    = (Temps.fresh_var Cil.voidType).Cil.vname in
          let gen_spec n t = (n, t, None, [], (G.Ppt.loc ppt)) in
          Cil.mkCompInfo true comp_name 
            begin fun ci ->
              let spec_of_payload = function
                | Var    (n,t) -> gen_spec n t
                | Handle (n,t) -> gen_spec n t
              in 
              (List.map spec_of_payload (header @ payload)) 
            end []
        in                        
        
        assert ci.cdefined ;
        assert (ci.cfields <> [] );

        (* Trace node type, and two variables that points to trace nodes. *)
        let trnd_typ  = Cil.TComp (ci, []) in 
        let trnd_var0 = Temps.fresh_var (Cil.TPtr(trnd_typ, [])) in (* trace node ptr *)
        let trnd_var1 = Temps.fresh_var (Cil.TPtr(trnd_typ, [])) in (* trace node ptr *)
        
        (* Recovery function fills holes in the code with pures--pures
           whose construction had to be delayed until now. Why?
           Because we cannot generate temporary variables until we
           have gathered enough information to generate their types.
           In the case of a trace node, its type depends on its
           (entire) payload.  This causes a circular dependency that
           we break by naming "holes" in the code and filling them
           with this function below. *)
        let recov : recov = 
          let map : (G.pure -> G.pure) Strmap.t =
            List.fold_left2 begin fun map p f ->
              let lval_w_hole x = (Cil.Mem x, Cil.Field (f, Cil.NoOffset)) in
              let name, pure_w_hole = match p with
                | Var     (n,_) -> n, (fun x -> Cil.Lval   (lval_w_hole x))
                | Handle  (n,_) -> n, (fun x -> Cil.AddrOf (lval_w_hole x))
              in
              (Strmap.add name pure_w_hole map)
            end Strmap.empty (header @ payload) ci.Cil.cfields
          in
          let find n = 
            if true 
            then Strmap.find n map
            else
              try (* DEBUG/TEMP *)
                Strmap.find n map with Not_found ->
                  let _ = ignore (log "Ceal Internal Bug: Couldn't find name %s\n" n) in
                  fun _ -> null
          in
          let trnd0 = Cil.mkCast ~e:(pure_of_var trnd_var0) ~newt:Cil.voidPtrType in
          (* let trnd1 = Cil.mkCast ~e:(pure_of_var trnd_var1) ~newt:Cil.voidPtrType in *)
          let trnd1 = pure_of_var trnd_var1 in
          function
            | Null_hole                       -> null
            | Trnd0                           -> trnd0
            | ( Trnd0_payload (Var    (n,_))
              | Trnd0_payload (Handle (n,_))) -> (find n) (pure_of_var trnd_var0)
                
            | Trnd1                           -> trnd1
            | ( Trnd1_payload (Var    (n,_))
              | Trnd1_payload (Handle (n,_))) -> (find n) (pure_of_var trnd_var1)
        in
        
        (* Program combinators (and a pure) for a new function *)
        let func_combs (*(tag:[`Redo | `Undo])*) (nodes: recov -> G.nodes) (rtypes:Cil.typ list) 
            : P.comb * P.comb * G.pure = 
          if nodes recov G.emptyz = G.emptyz
          then 
            let id = fun z -> z in 
            (id, id, null)
          else 
            let var   = Temps.func_var [trnd_var0] rtypes in
            (* TEMP / DEBUG *)
            (* ignore (log "trnode_begin : %s\n" var.Cil.vname) ; *)
            let graph = G.unfocus (nodes recov G.emptyz) in
            let func  = (var, [trnd_var0], graph, G.Ppt.from ppt) in
            (P.Build.vard ppt var,
             P.Build.func ppt func,
             pure_of_var var)
        in
        
        (* Create undo and redo functions *)      
        (* Regarding return types, Note that:
           (1) redo functions may return type sequences (necessary for well-typedness?)
           (2) undo functions always return void. 
        *)
        let redo_decl, redo_func, redo_pure = func_combs below.redo (rtypes ppt) in
        let undo_decl, undo_func, undo_pure = func_combs below.undo [] in
        
        let cast type_spec e = 
          Cil.mkCast ~e:e ~newt:(L.type_of_type_spec ppt type_spec) in
        
        let desc_globals, desc_ptr = 
          Trnode_descriptor.to_cil 
            { Trnode_descriptor.ppt   = ppt ;
              Trnode_descriptor.size  = Cil.sizeOf trnd_typ ;
              Trnode_descriptor.flags = desc_flags ;
              Trnode_descriptor.redo  = cast Low.T_redofn redo_pure ;
              Trnode_descriptor.undo  = cast Low.T_undofn undo_pure ; 
            }
        in
        
        let nodes = B.let_simp ppt trnd_var0 
          (G.Low (L.Trace_node, [desc_ptr]))
        in
        
        let sep_comment = "/* - - - - - - - - - - - - - - - - - - - " ^
          "- - - - - - - - - - - - - - - - - - */" in

        { data  = (fun p -> p) ;
          flags = default_flags ;
          decls = (fun z -> (begin P.Build.cil ppt (Cil.GText sep_comment)
                               **> P.Build.cil ppt (Cil.GCompTag (ci, (G.Ppt.loc ppt)))
                               **> redo_decl 
                               **> undo_decl 
                               **> List.fold_right (P.Build.cil ppt) desc_globals
                               **> P.Build.cil ppt (Cil.GText sep_comment)
                               **> below.decls 
                               **> z 
                             end)) ;

          funcs = (fun z -> (begin redo_func 
                               **> undo_func 
                               **> below.funcs 
                               **> z 
                             end)) ;

          whole = (fun _ z -> nodes **> below.whole recov **> z ) ;
          apart = (fun _ z -> nodes **> below.apart recov **> z ) ;
          redo  = (fun _ z -> z ) ;
          undo  = (fun _ z -> z ) ;
          rvars = below.rvars ;
          rlets = below.rlets ;
        } 
      in
      (fun above -> {above with mode=Traced (Pre_redo, [])}, doit)
    in
    
    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Save live variables } *)
    let trnode_save_live (ppt:G.ppt) (load:bool) : (above, below) W.walk_comb = 
      fun above -> match above.mode with
        | Untraced -> assert false
        | Traced (phase, vars (* invariant: these vars are already saved. *)) ->
            let ( -- ) = Varinfo.Set.diff in
            let ( ++ ) = Varinfo.Set.union in
            let elem   = Varinfo.Set.elements in
            let set    = Varinfo.set_from_list in
            let live   = live_vars_at ppt in 
            let save   = elem ((set live) -- (set vars)) (* additional vars to save *) in
            let all    = elem ((set live) ++ (set vars)) (* _all_ vars saved so far *) in
            let save_vars = fun r z -> (* save additional vars *)
              List.fold_right begin fun var z ->
                B.write ppt (lval_of_pure (r (fst (allot_var var)))) (pure_of_var var) **> z
              end save **> z
            in             
            let load_vars = fun r z -> (* load all live variables *)
              if not load then z else
                List.fold_right begin fun var z ->
                  B.let_simp ppt var (G.Pure (r (fst (allot_var var)))) **> z
                end live **> z
            in
            ({above with mode=Traced (phase, all)}, fun below -> 
               { below with 
                   data  = (fun p -> (List.map (fun v -> (snd (allot_var v))) save) @ (below.data p) ) ;
                   whole = (fun r z -> save_vars r **> below.whole r **> z ) ;
                   apart = (fun r z -> save_vars r **> below.apart r **> z ) ;
                   redo  = (fun r z -> load_vars r **> below.redo  r **> z ) ;
               })
    in


    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Insert a memoization point. } *)
    let trnode_memo_test (ppt:G.ppt) (scope:G.scope) : (above, below) W.walk_comb = 
      let h, p = allot_handle ppt L.T_memoh in 
      (* Invoke memoization, conditionally branch on the
         result (to test for memo match). *)
      let invoke below = fun r z ->
        (* address of keys *)
        let bytes : recov -> G.pure = 
          fun r -> match (live_vars_at ppt) with 
            | []   -> null (* void pointer; no vars in payload *)
            | v::_ -> begin match r (fst (allot_var v)) with                       
                  (* address of first var in payload *)
                | Cil.Lval lval -> Cil.AddrOf lval
                | _             -> assert false
              end
        in
        (* byte count of keys *)
        let bytec : recov -> G.pure = fun _ -> 
          Cil.integer 
            (List.fold_left 
               (fun bytec v -> bytec + ((Cil.bitsSizeOf v.Cil.vtype) lsr 3))
               0 (live_vars_at ppt))
        in
        let gen_label_info _ = 
          let l = Temps.fresh_label (G.Ppt.loc ppt) in 
          l, (l, [], [], G.Ppt.from ppt) 
        in
        let skip, skip_info = gen_label_info () in
        let body, body_info = gen_label_info () in                
        let memo_table_ptr = match scope with 
          | G.Scope_change ptr -> ptr
          | G.Scope_same       -> null 
        in
        let args  = [r Trnd0; r h; memo_table_ptr; bytes r; bytec r] in
        let rvars = match below.rvars with
          | None    -> []
          | Some vs -> vs
        in
        begin B.let_simp ppt (var_of_pure (r Trnd1)) (G.Low (L.Memo_invoke (live_vars_at ppt), args))
          **> B.let_blocks ppt (
            [ skip_info, begin fun z -> 
                (* executed when there is a match; skips body. *)
                begin List.fold_right begin fun v z ->
                  try
                    (* Find code to bind each return var by using [below.rlets] *)
                    (Varmap.find v below.rlets) r **> z 
                  with
                      (* DEBUG/TEMP *)
                      Not_found -> let _ = ignore 
                        (log "%s Can't find 'rvar' in 'rlets': %s\n" 
                           (G.Ppt.long_string ppt) v.Cil.vname)
                      in 
                      z (* raise Not_found *)
                end rvars
                end
                **> let ppt' = G.Ppt.set_props 
                  (fun props -> G.Property.Memo_code :: props ) ppt 
                in
                B.return ppt' (List.map pure_of_var rvars)
                **> z 
              end
              ;
              (* body of memo point; executed when there is no match *)
              body_info, fun _ -> z 
            ])
          **> B.cond ppt (r Trnd1) (G.Br (skip,[])) (G.Br (body,[]))
          **> G.emptyz
        end
      in
      let revoke = fun r -> 
        let args = [r Trnd0; r h] in
        B.let_simp ppt wc_var (G.Low (L.Memo_revoke, args))
      in
      begin fun above -> above, fun below -> 
        let invoke' = invoke below in 
        { below with
            data  = (fun z   -> p :: (below.data z)) ;
            flags = { below.flags with flag_memo = true } ;
            whole = (fun r z -> invoke' r **> below.whole r **> z ) ;
            apart = (fun r z -> invoke' r **> below.apart r **> z ) ;
            undo  = (fun r z -> revoke  r **> below.undo  r **> z ) }
      end
    in


    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Save a rlet } *)
    let trnode_add_rlet (ppt:G.ppt) (var:G.var) (nodes:recov -> G.nodes) 
        : (above, below) W.walk_comb = 
      fun a -> a, fun b -> { b with rlets = Varmap.add var nodes b.rlets } 
    in

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Save a rlet } *)
    let trnode_set_rvars (ppt:G.ppt) (vars:G.vars) =       
      fun above -> above, fun below -> 
        { below with rvars = Some vars }
    in

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Set a flag } *)
    let trnode_set_flag (flag:[`Unboxed | `Read | `Write])
        : (above, below) W.walk_comb = 
      fun a -> a, fun b -> { b with flags = 
          { b.flags with
              flag_unboxed = b.flags.flag_unboxed || (flag = `Unboxed) ;
              flag_read    = b.flags.flag_read    || (flag = `Read)    ;
              flag_write   = b.flags.flag_write   || (flag = `Write)   ;
          } }
    in
    
    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Switch to post_redo phase } *)
    let trnode_begin_redo (ppt:G.ppt) : (above, below) W.walk_comb = 
      let h, p = allot_handle ppt L.T_updateh in 
      let args = fun r -> [r Trnd0; r h] in
      let invoke = fun r -> B.let_simp ppt wc_var (G.Low (L.Update_invoke [], args r )) in
      let revoke = fun r -> B.let_simp ppt wc_var (G.Low (L.Update_revoke   , args r )) in
      fun above -> match above.mode with
        | Untraced         -> assert false
        | Traced (_, vars) -> {above with mode=Traced (Post_redo, vars)}, 
            fun below -> 
              { below with 
                  data  = (fun z -> p :: below.data z ) ;
                  whole = (fun r z -> invoke r **> below.whole r **> z ) ;
                  apart = (fun r z -> invoke r **> below.apart r **> z ) ;
                  undo  = (fun r z -> revoke r **> below.undo  r **> z ) }
    in

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 End a trace node} *)
    let trnode_end (ppt:G.ppt) : (above, below) W.walk_comb = 
      fun above -> 
        { above with mode = Untraced }, 


        fun below ->
          let apart_decl, apart_func, apart_tcall =
            (* For scoping the tcall in the redo code, we rely on
               the invariant that if we are in the redo phase, then
               all the live vars needed by the tail call have
               already been restored. *)
            let (var,vars,_,_) as func = 
              let graph = G.unfocus (below.apart recov_none **> G.emptyz) in
              let vars = live_vars_at ppt in
              (* TODO-20-Nov-2009 : voidType isn't necessarily right here. *)
              let var = Temps.func_var vars (rtypes ppt) in
              (* DEBUG -- Useful point to inspect live variable info: *)
              let _ = if ! Global_flags.debug_live_vars then ignore **>
                log "trnode_end %a. created fun %s. live = {%a}@!"
                G.Ppt.doc ppt
                var.Cil.vname
                G.Print.pr_vars_doc vars
              in
              (var, vars, graph, G.Ppt.from ppt)
            in
            let pure  = pure_of_var var in
            let pures = List.map pure_of_var vars in
            begin
              (* apart_decl  *) P.Build.vard ppt var,
              (* apart_func  *) P.Build.func ppt func,
              (* apart_tcall *) (fun r -> B.call ppt G.Default pure pures)
            end
          in
        { below with

            decls = begin match above.mode, above.need_apart with
              | ( (Traced (Post_redo, _), _)
                | (_, Apart_yes) ) -> (fun z -> apart_decl **> below.decls **> z)
              | _                  -> below.decls
            end ;
                  
            flags = { below.flags with flag_return = false } ;
            
            funcs = begin match above.mode, above.need_apart with
              | ( (Traced (Post_redo, _), _)
                | (_, Apart_yes) ) -> (fun z -> apart_func **> below.funcs **> z) 
              | _                  -> below.funcs
            end ;

            apart = begin match above.need_apart with
              | Apart_no  -> below.apart
              | Apart_yes -> apart_tcall
            end ;

            redo = begin match above.mode with 
              | ( Untraced | (Traced (Pre_redo, _))) -> below.redo
              | Traced (Post_redo, _) -> apart_tcall
            end ;
            
            undo = 
            let ppt = 
              (G.Ppt.set_props (fun props -> G.Property.Undo_code :: props) ppt) 
            in
            (fun _ -> B.return ppt [] ) ;
        }
    in

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 Insert nodes into output} *)    
    let rec insert (ppt : G.ppt) 
        ( mode  : (* What constraints are there are these nodes? *)
            [ `Untraced  (* nodes must be inserted in untraced mode *)
            | `Traced of (* nodes must be inserted in a traced mode *)
                [ `Pre_redo      (* Must be in pre-redo phase. Can be a singleton. *)
                | `Pre_redo_dest (* A destination allocation. Can NOT be singleton. *)
                | `Post_redo     (* Must be in post-redo phase. Can be a singleton *)
                | `Either        (* Can be in either pre- or post-redo phase, Can be a singleton. *)
                ]
            ] option (* None --> no constraint; Some --> some constraint. *)
        )
        ( data  : payload list )
        ( whole : recov -> G.nodes )
        ( apart : recov -> G.nodes )
        ( redo  : recov -> G.nodes )
        ( undo  : recov -> G.nodes )
        : (above,below) W.walk_comb 
        =       
      
      (* Insert the given nodes, perhaps in a different mode than the
         current one.  We delay the node insertion until we can switch
         modes, as needed. *)
      let insert_nodes : (above,below) W.walk_comb  = 
        let ins (above:above) (below:below) : below = 
          let insert_untraced (below:below) : below = 
            { below with
                data  = (fun p   -> data @ (below.data p)) ;
                whole = (fun r z -> whole r **> below.whole r **> z ) ;
                apart = (fun r z -> apart r **> below.apart r **> z ) }
          in
          match above.mode with
            | Untraced -> insert_untraced below
                
            | Traced (Pre_redo, _) -> insert_untraced 
                { below with  (* no redo yet, just undo code. *)
                    undo  = (fun r z -> undo r **> below.undo r **> z ) }
                  
            | Traced (Post_redo, _) -> insert_untraced
                { below with (* both undo and redo code. *)
                    redo = (fun r z -> redo r **> below.redo r **> z ) ;
                    undo = (fun r z -> undo r **> below.undo r **> z ) }                
        in
        fun above -> (above, ins above)
      in
      
      (* Do insertion, switching modes as necessary *)
      fun above -> match (! Global_flags.singleton_trnodes, above.mode, mode) with
          
        (* Case S1: Enforce singletons; insert `Pre_redo code. *)
        | true, Traced _, Some (`Traced ( `Either | `Pre_redo )) ->
            begin trnode_end   ppt
              @@> trnode_begin ppt
              @@> insert_nodes
            end above

        (* Case S2: Enforce singletons; insert `Post_redo code. *)
        | true, Traced _, Some (`Traced ( `Post_redo )) ->
            begin trnode_end        ppt
              @@> trnode_begin      ppt
              @@> trnode_begin_redo ppt
              @@> trnode_save_live  ppt true
              @@> insert_nodes
            end above

        (* Case S3: Enforce singletons, insert `Untraced code. *)
        | true, Traced _, None ->
            begin trnode_end ppt
              @@> insert_nodes
            end above

        (* Case A: Mode is unchanged. *)
        | ( _,     Untraced,              Some `Untraced
          | _,     Untraced,              None
          | false, Traced _,              None
          | false, Traced _,              Some (`Traced `Either)
          | false, Traced (Pre_redo,  _), Some (`Traced `Pre_redo)
          | _,     Traced (Pre_redo,  _), Some (`Traced `Pre_redo_dest)
          | false, Traced (Post_redo, _), Some (`Traced `Post_redo)
          ) -> insert_nodes above
            
        (* Case B: Untraced  --> Pre_redo *)
        | _, Untraced, Some (`Traced (`Pre_redo | `Pre_redo_dest | `Either)) -> 
            begin trnode_begin ppt
              @@> insert_nodes 
            end above
              
        (* Case C: Untraced  --> Post_redo *)
        | _, Untraced, Some (`Traced `Post_redo) -> 
            begin trnode_begin      ppt
              @@> trnode_begin_redo ppt
              @@> trnode_save_live  ppt true
              @@> insert_nodes
            end above

        (* Case D: Pre_redo  --> Untraced *)
        (* Case E: Post_redo --> Untraced *)
        | _, Traced _, Some `Untraced -> 
            begin trnode_end ppt
              @@> insert_nodes
            end above
              
        (* Case F: Pre_redo --> Post_redo *)
        | false, Traced (Pre_redo, vars), Some (`Traced `Post_redo) -> 
            begin trnode_begin_redo ppt
              @@> trnode_save_live  ppt true 
              @@> insert_nodes 
            end above
              
        (* Case G: Post_redo --> Pre_redo *)
        | _, Traced (Post_redo, _), Some (`Traced `Pre_redo) ->
            begin trnode_end   ppt 
              @@> trnode_begin ppt
              @@> insert_nodes
            end above
              
        (* Impossible. *)
        | _, Traced (Post_redo, _), Some (`Traced `Pre_redo_dest) ->
            (* This should never happen. *)
            assert false
    in    

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (** {3 walk functions } *)

    let lower_first = function
      | G.Entry -> begin fun above ->
          match above.mode with
            | Untraced -> (above, fun b -> b)
            | _        -> assert false
        end (* function *)
          
      | G.Label (label, labels, vars, ppt) ->
          let cons_label = fun r z ->
            (* make new program points distict from old ones *)
            B.cons_first (G.Label (label, labels, vars, G.Ppt.from ppt)) **> z
          in
          (* Is this label enterable from the redo code? *)
          begin match func_of_label label with            
              (* No, label is not enterable from redo code *)
            | None -> fun above ->
                insert ppt None [] 
                  cons_label cons_label empty empty
                  { above with need_apart = Apart_no }
                  
            (* Yes, label is enterable from redo code *)
            | Some (fun_var, fun_vars, body) -> fun above ->
                ({above with need_apart=Apart_yes; mode=Untraced}, fun below ->
                   let apartf = 
                     (fun_var, fun_vars,
                      G.unfocus (body **> below.apart recov_none **> G.emptyz),
                      G.Ppt.from ppt)
                   in
                   let tcall_nodes = fun r ->
                     B.call ppt G.Default (pure_of_var fun_var)
                       (lift_label_pures label (List.map pure_of_var vars))
                   in
                   { below with
                       decls = (fun z -> P.Build.vard ppt fun_var **> below.decls **> z) ;
                       funcs = (fun z -> P.Build.func ppt apartf  **> below.funcs **> z) ;
                       whole = (fun r z -> cons_label r **> below.whole r **> z) ;
                       apart = (fun r z -> cons_label r **> tcall_nodes r **> z) ;
                       undo  = empty ;
                       redo  = begin match above.mode with
                         | Traced (Post_redo, _) -> (fun r z -> cons_label r **> tcall_nodes r **> z)
                         | _                     -> empty
                       end ;
                   } )
          end (* match *)
    in

    (* Change a branch into a function call, 
       if the target label is cross-unit. *)
    let apart_branch
        (ppt:G.ppt) (label:G.label) (pures:G.pure list) 
        : recov -> G.nodes 
      =
      match func_of_label label with
        | None -> (fun r -> B.branch ppt (G.Br (label, pures)))
        | Some (fun_var, _, _) ->
            let pure  = pure_of_var fun_var in
            let pures = lift_label_pures label pures in
            (fun r -> B.call ppt G.Default pure pures)
    in

    let lower_nested (labels:G.label list) (above:above) =
      let merge_below b1 b2 =
        let rvars = match b1.rvars, b2.rvars with
          | None,    None    -> None
          | Some vs, None    -> Some vs
          | None,    Some vs -> Some vs
          | Some vs, Some ws -> 
              let _ = List.iter2 (fun v w -> assert (v == w)) vs ws in Some vs
        in
        { decls = (fun z -> b1.decls **> b2.decls **> z ) ;
          flags = b1.flags (* Note: we drop b2.flags here. *) ;
          funcs = (fun z -> b1.funcs **> b2.funcs **> z ) ;
          data  = (fun p -> b1.data  **> b2.data  **> p ) ;
          whole = (fun r -> G.merge_nodes (b1.whole r) (b2.whole r)) ;
          apart = (fun r -> G.merge_nodes (b1.apart r) (b2.apart r)) ; 
          redo  = (fun r -> G.merge_empty (b1.redo  r) (b2.redo  r)) ;
          undo  = b1.undo (* Note: we drop b2.undo here. *) ;
          rvars = rvars ;
          rlets = Varmap.fold (fun v n m -> Varmap.add v n m) b1.rlets b2.rlets ;
        }
      in
      (* FIX: used to be {above with mode=Untraced}.  Was this necessary? *)
      (above, fun nested below -> List.fold_left merge_below below nested)
      (* ( { above with need_apart = Apart_no }, 
        fun nested below -> List.fold_left merge_below below nested ) *)
    in
    
    let lower_middle = function
      | G.Let_block (labels, ppt) ->
          let nodes = (fun _ -> B.let_blocks'' ppt labels) in
          insert ppt (Some `Untraced) []
            nodes nodes empty empty
            

      | G.Let_simp (var, simp, ppt) -> begin match simp with
          | G.Low _ -> (* TODO: What is the right behavior here? *)
              let nodes = (fun _ -> B.let_simp ppt var simp) in
              insert ppt None [] nodes nodes nodes nodes
                
          | G.Clib (pure, pures) ->
              let nodes = (fun _ -> B.let_simp ppt var simp) in
              insert ppt None [] nodes nodes nodes empty

          | G.Ffi (Ffi.Srcvar, op, pures) ->
              let phase, set_flags = 
                (* Can this code wake up?
                   (i.e., does it contain an implicit update point?) *)
                ( if op.Ffi.flags.Ffi.wakeup 
                  then Some (`Traced `Post_redo), trnode_set_flag `Read
                  else Some (`Traced `Either),    W.id 
                ) 
              in
              let h, payload = 
                match op.Ffi.payload with 
                  | Some typ when 
                      Cil.typeSig typ <> Cil.TSBase(Cil.voidType) 
                      -> let h, p = allot_space typ in h, [p]
                  | _ -> (Null_hole, [])
              in
              let args  = fun r -> (r Trnd0 :: r h :: pures) in
              let args' = fun r -> (r Trnd0 :: r h :: []) in
              begin
                insert ppt phase payload
                  (fun r -> B.let_simp ppt var    (G.Ffi (Ffi.Invoke, op, (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Ffi (Ffi.Invoke, op, (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Ffi (Ffi.Revinv, op, (args  r))))
                  (fun r -> B.let_simp ppt wc_var (G.Ffi (Ffi.Revoke, op, (args' r))))
                @@> set_flags
              end
                  
          | G.Ffi (_, op, pures) ->
              assert false

          | (G.Peek _ | G.Poke _) ->
              let nodes = (fun _ -> B.let_simp ppt var simp) in
              insert ppt None [] nodes nodes nodes empty
                
          | G.Pure pure -> 
              let nodes = (fun _ -> B.let_simp ppt var (G.Pure pure)) in
              insert ppt None [] nodes nodes nodes empty
                
          | G.Read lval ->              
              let _ = if false then
                Printf.fprintf stderr "lowering read at : %s\n" (G.Ppt.long_string ppt)
              in
              let q, t, ptr = L.qual_basetype_pointer_of_lval lval in
              let h, p      = allot_handle ppt (L.T_readh (q,t)) in
              let args      = fun r -> [r Trnd0; r h; ptr] in
              let args'     = fun r -> [r Trnd0; r h] in
              begin 
                insert ppt (Some (`Traced `Post_redo)) [p]
                  (fun r -> B.let_simp ppt var    (G.Low (L.Read_invoke (q,t), (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Read_invoke (q,t), (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Read_revinv (q,t), (args  r))))
                  (fun r -> B.let_simp ppt wc_var (G.Low (L.Read_revoke (q,t), (args' r))))
                @@> trnode_set_flag `Read
              end

          | G.Scope ->
              let h, p = allot_handle ppt L.T_scopeh in
              let args = fun r -> [r Trnd0; r h] in
              begin insert ppt (Some (`Traced `Either)) [p]
                  (fun r -> B.let_simp ppt var    (G.Low (L.Scope_invoke, (args r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Scope_invoke, (args r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Scope_revinv, (args r))))
                  (fun r -> B.let_simp ppt wc_var (G.Low (L.Scope_revoke, (args r))))
                @@> trnode_set_flag `Unboxed
              end

          | G.Alloc typ -> 
              let pre_redo _ = 
                let h, p = allot_space typ in
                let args = fun r -> [r Trnd0; r h; Cil.SizeOf typ] in
                insert ppt (Some (`Traced `Pre_redo_dest)) [p]
                  (fun r -> B.let_simp ppt var (G.Low (L.Unboxed_invoke, (args r))))
                  (fun r -> B.let_simp ppt var (G.Low (L.Unboxed_invoke, (args r))))
                  (fun r -> fun z -> if z = G.emptyz then z else
                     B.let_simp ppt var (G.Low (L.Unboxed_revinv, (args r))) z)
                  (fun r -> B.let_simp ppt var (G.Low (L.Unboxed_revoke, (args r))))
                @@> trnode_set_flag `Unboxed
                @@> trnode_add_rlet ppt var begin fun r ->
                  B.let_simp ppt var (G.Pure (r (Trnd1_payload p)))
                end
              in
              let post_redo _ =
                let h, p  = allot_handle ppt L.T_alloch in
                let args  = fun r -> [r Trnd0; r h; Cil.SizeOf typ] in
                let args' = fun r -> [r Trnd0; r h] in
                insert ppt (Some (`Traced `Either)) [p]
                  (fun r -> B.let_simp ppt var    (G.Low (L.Alloc_invoke, (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Alloc_invoke, (args  r))))
                  (fun r -> B.let_simp ppt var    (G.Low (L.Alloc_revinv, (args  r))))
                  (fun r -> B.let_simp ppt wc_var (G.Low (L.Alloc_revoke, (args' r))))
              in
              fun above -> begin match above.mode with
                | Untraced              -> trnode_begin ppt @@> pre_redo ()
                | Traced (Pre_redo, _)  -> pre_redo ()
                | Traced (Post_redo, _) -> post_redo ()
              end above


        end (* match simp *)
          
      | G.Write (lval, pure, ppt) ->
          let q, t, ptr = L.qual_basetype_pointer_of_lval lval in
          let h, p      = allot_handle ppt (L.T_writeh (q,t)) in
          let args      = fun r -> [r Trnd0; r h; ptr; pure] in
          let args'     = fun r -> [r Trnd0; r h] in
          begin 
            insert ppt (Some (`Traced `Either)) [p] 
              (fun r -> B.let_simp ppt wc_var (G.Low (L.Write_invoke (q,t), (args  r))))
              (fun r -> B.let_simp ppt wc_var (G.Low (L.Write_invoke (q,t), (args  r))))
              (fun r -> B.let_simp ppt wc_var (G.Low (L.Write_revinv (q,t), (args  r))))
              (fun r -> B.let_simp ppt wc_var (G.Low (L.Write_revoke (q,t), (args' r))))
            @@> trnode_set_flag `Write
          end
            
      | G.Memo (scope, ppt) ->
          (* TODO -- currently, this unconditionally starts a new
             trnode; but this is only necessary in some cases.  It is
             unnecessary if the phase is still pre-redo and the
             trace-node began with a memo *)
          begin insert ppt (Some (`Untraced)) [] empty empty empty empty
            @@> trnode_begin     ppt
            @@> trnode_save_live ppt false
            @@> trnode_memo_test ppt scope
          end
            
      | G.Update ppt -> failwith "Shouldn't happen" (*
          let h, p = allot_handle ppt L.T_updateh in 
          let args = fun r -> [r Trnd0; r h] in
          insert ppt (Some (`Traced `Pre_redo)) [p]
            (fun r -> B.let_simp ppt wc_var (G.Low (L.Update_invoke [], args r )))
            (fun r -> B.let_simp ppt wc_var (G.Low (L.Update_invoke [], args r )))
            empty 
            (fun r -> B.let_simp ppt wc_var (G.Low (L.Update_revoke   , args r )))
                                                    *)

      | G.Rvars (rvars, ppt) ->
          trnode_set_rvars ppt rvars

      | G.Cut (label, memo, ppt) ->
          let clear_rvars = fun a -> a, 
            fun b -> {b with rvars = None} 
          in          
          let set_region ppt = fun a -> 
            {a with rmode = R.rmode_of_cut ppt rmodes},
            fun b -> b 
          in
          (* Wraps the cut region with calls to Cut_begin / Cut_end *)          
          (* Turns the return point into a cross-unit call, if needbe. *)
          (* Ignoring the lowering of memo points, 
             and ignoring that L could be changed into a tail call, 
             here's the idea:

             [[ L(cut memo? e) ]] 
             =def=
             letblk L'(x) =
             let _ = cut_begin () in
             L(x)
             in
             L( cut let _ = cut_begin () in                 
             [[ memo? e ]] 
             )
          *)
          let cut (is_apart:bool) r = 
            let label'   = Temps.fresh_label (G.Ppt.loc ppt) in
            let formals' = List.map 
              (fun v -> Temps.fresh_var v.Cil.vtype) 
              (G.formals_of_label label graph)
            in
            begin fun z ->
              B.let_block ppt label' formals' begin 
                fun z -> B.let_simp ppt wc_var (G.Low (L.Cut_end, []))
                  **> begin
                    let pures = List.map pure_of_var formals' in
                    if is_apart 
                    then apart_branch ppt label pures r
                    else B.branch ppt (G.Br (label, pures))
                  end **> z
              end
              **> B.let_simp ppt wc_var (G.Low (L.Cut_begin, []))
              **> B.cut ppt label' (G.Memo_no)
              **> z
            end
          in
          
          let cut_whole = cut false in
          let cut_apart = cut true  in

          begin match memo with 
              (* TODO: check for existence of one or more live variables too? *)
            | G.Memo_no ->        
                begin clear_rvars
                  @@> set_region ppt
                  @@> insert ppt (Some `Untraced) [] 
                  cut_whole cut_apart empty empty
                end
                  
            | G.Memo_yes (memo_ppt, scope) ->
                begin clear_rvars 
                  @@> set_region ppt
                  @@> insert ppt (Some (`Untraced)) [] 
                  cut_whole cut_apart empty empty
                  @@> trnode_begin     memo_ppt 
                  @@> trnode_save_live memo_ppt false
                  @@> trnode_memo_test memo_ppt scope
                end
          end (* match memo *)
    in
    
    let lower_last last =
      let insert_last ppt mode payload whole apart redo undo above =
        (snd (insert ppt mode payload whole apart redo undo above)) empty_below
      in
      let undo_return ppt : recov -> G.nodes = 
        fun _ -> B.return (G.Ppt.set_props (fun props -> G.Property.Undo_code :: props) ppt) [] 
      in
      match last with
        | G.Exit -> (fun _ -> empty_below)
            
        | G.Cond (pure, br1, br2, ppt) ->
            (* Are the targets of the branches going to become
               functions?  If so, we need to create new target labels
               that will call these functions. *)
            let make_apart ((G.Br (label, pures)) as br) 
                : (recov -> G.nodes) * G.branch 
                = match func_of_label label with
                  | None   -> (fun r z -> z), br
                  | Some _ ->
                      let label' = Temps.fresh_label (G.Ppt.loc ppt) in
                      begin fun r -> 
                        B.let_block ppt label' [] 
                          (apart_branch ppt label pures r)
                      end, 
                      G.Br (label', [])
            in
            let apart = 
              let nodes1, br1' = make_apart br1 in
              let nodes2, br2' = make_apart br2 in
              fun r z -> 
                begin nodes1 r 
                  **> nodes2 r 
                  **> B.cond ppt pure br1' br2'
                  **> z
                end
            in
            let whole = fun r -> B.cond ppt pure br1 br2 in
            insert_last ppt None [] whole apart apart (undo_return ppt)
              
        | G.Call (mode, pure, pures, ppt) ->
            (* TODO: does the call-mode matter? *)
            let nodes = fun r -> B.call ppt mode pure pures in
            insert_last ppt None [] nodes nodes nodes (undo_return ppt)
              
        | G.Branch (G.Br(label,pures) as br, ppt) ->
            let whole = fun r -> B.branch ppt br in
            let apart = apart_branch ppt label pures in
            insert_last ppt None [] whole apart apart (undo_return ppt)
              
        | G.Return (pures, ppt) ->
            let nodes = 
              fun r -> B.return ppt pures 
            in 
            let set_rvars = fun above -> above, fun below -> 
              { below with rvars = Some (List.map var_of_pure pures) }
            in
            let set_is_return_to_true = fun above -> above, fun below -> 
              { below with flags = { below.flags with flag_return = true } }
            in
            fun above -> begin snd **> 
                begin match above.rmode with
                  | R.Direct ->
                      (* Direct return, nothing special here. *)
                      begin set_is_return_to_true 
                        @@> insert ppt None [] nodes nodes nodes (undo_return ppt)
                      end 
                        
                  | R.Indir_stable | R.Indir_change -> 
                      (* Region was DPS-xformed; We must set rvars. *)
                      begin insert ppt None [] nodes nodes nodes (undo_return ppt)
                        @@> set_is_return_to_true
                        @@> set_rvars 
                      end 
                end (* match *) above
            end empty_below
    in
    { W.walk_first  = lower_first ;
      W.walk_middle = lower_middle ;
      W.walk_nested = lower_nested ;
      W.walk_last   = lower_last ;
    }

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Entrances} *)
    
  module Entrances : sig
    type t

    val of_graph    : G.t -> out_channel option -> t
    val points      : t -> G.ppt list
    val is_entrance : G.ppt -> t -> bool
  end
    = 
  struct
    (* fst is all program points, snd are those that are entrances *)
    type t = G.Ppt.Set.t * G.Ppt.Set.t

    (* Returns a function that decides whether a program point is an
       entrance.  Each new entrance must be placed into a separate
       function by the normalization algorithm.
       
       In technical terms, an "entrance" is a program point whose
       immediate dominator is the root node (all with respect to the
       "augmented" control flow graph). *)    
    let of_graph (graph:G.t) (out_op:out_channel option) : t =      
      let dom_summary = Dom_summary.of_graph true out_op graph in
      let entrances = match dom_summary.Dom_summary.dom_tree with
        | Dom_summary.Dom_tree (_, subtrees) -> 
            List.fold_left begin fun entrances -> 
              function Dom_summary.Dom_tree ((pp, _), _) -> 
                G.Ppt.Set.add pp entrances
            end G.Ppt.Set.empty subtrees
      in
      (G.Ppt.set_from_list dom_summary.Dom_summary.pts, entrances)

    let points ((points, _):t) = G.Ppt.Set.elements points

    let is_entrance (ppt:G.ppt) ((_, entrances):t) =
      G.Ppt.Set.mem ppt entrances

  end (* Entrances module *)

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Lower a (core) function} *)
    
  (* Put a graph in normal form *)
  let of_func (rmodes:R.t)
      (((fun_var, fun_args, fun_graph, fun_ppt) as func) : P.func) : P.comb =
    
    (* let _ = Printf.fprintf stderr "lowering func : %s\n" (fun_var.vname) in *)

    let live_vars_at : G.ppt -> G.var list = 
      let live_vars = Live_vars.of_graph fun_args fun_graph in
      (fun ppt -> Live_vars.at_ppt ppt live_vars)
    in    

    let rtypes : G.ppt -> Cil.typ list =
      Return_types.of_func func
    in
    
    let func_of_label : G.label -> (G.var * G.var list * G.nodes) option =
      
      let entrances =        
        if ! Global_flags.debug_dom_trees 
        then
          let path_wo_ext = Filename.concat 
            (!Global_flags.output_path) 
            (fun_var.Cil.vname ^ ".dom_tree")
          in
          let path_dot = (path_wo_ext ^ ".dot") in
          let out = open_out path_dot in
          let entrances = (Entrances.of_graph fun_graph (Some out)) in
          let _ = close_out out in
          let _ = Dot_util.run path_wo_ext "ps"  "ps" in
          let _ = Dot_util.run path_wo_ext "pdf" "pdf" in            
          ( entrances )
        else
          (Entrances.of_graph fun_graph None)
      in

      let func_map  = List.fold_left begin fun func_map label ->
        let _,labels,vars,ppt = G.info_of_label label fun_graph in
        let func_op = if (Entrances.is_entrance ppt entrances) then
          let vars'  = List.map Temps.fresh_ver vars in
          
          (* let vars'' = lift_label_gen fun_graph live_vars_at Temps.fresh_ver label vars' in *)
          let vars'' = lift_label_gen fun_graph live_vars_at (fun v -> v) label vars' in

          let body   = (fun z -> B.let_blocks' ppt [((label,labels,vars,ppt), G.unfocus z)]
                          **> B.branch ppt (G.Br(label, List.map pure_of_var vars'))
                          **> G.emptyz) in

          let fun_var  = Temps.func_var vars'' (rtypes ppt) in
          (* DEBUG -- Useful point to inspect live variable info: *)
          let _ = 
            if   ! Global_flags.debug_live_vars
              || ! Global_flags.debug_entrances 
            then 
              ignore **>
                Pretty.fprint stderr ~width:80 
                (Pretty.dprintf "func_of_label %a. created func %s from label %s. live = {%a}@!"
                   G.Ppt.doc ppt
                   fun_var.Cil.vname
                   (string_of_label label)
                   G.Print.pr_vars_doc vars'')
          in
          Some (fun_var, vars'', body)
        else 
          None
        in 
        Label.Map.add label func_op func_map
      end (* fun *)
        Label.Map.empty 
        (List.filter 
           (fun label -> label <> G.entry_label) 
           (G.labels fun_graph))
      in 
      (fun label -> Label.Map.find label func_map)
    in

    (* Walk over the original function, generating new code *)
    let below = W.walk 
      (walk_fns live_vars_at func_of_label fun_graph rmodes rtypes)
      { mode       = Untraced ; 
        need_apart = Apart_no ;
        rmode      = R.rmode_of_func fun_var rmodes }
      (G.entry fun_graph)
    in        
    begin fun z ->
      below.decls 
      **> P.Build.func fun_ppt
        (fun_var, fun_args, 
         G.unfocus (below.whole recov_none **> G.emptyz),
         G.Ppt.from fun_ppt)
      **> below.funcs
      **> z
    end

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (** {3 Lower a (core) program} *)
      
  let of_program (rmodes:R.t) (prog:P.t) : P.comb =
    let walk_fns : (unit, P.comb) P.Walk.walk_fns = {
      P.Copy.walk_fns with 
        P.Walk.walk_glob = begin 
          fun glob a -> a,
            match glob with 
              | P.Func _ -> 
                  (* dont copy functions -- we'll expand them into more code. *)
                  (fun below -> below)
              
              | _ -> begin fun comb z -> 
                  (* copy all non-function globals. *)
                  P.Build.glob glob **> comb **> z 
                end 
        end ;

        P.Walk.walk_func = begin 
           fun func a -> a, 
             (* expand each global function into a sequence of globals
                (see of_func) *)
             fun below frag -> of_func rmodes func **> below **> frag
        end ; }
    in
    P.Walk.walk walk_fns () prog

end (* Lower_core module *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(** {3 Alpha-varies each local variable defined in the program,
    including parameters to functions.  Using this module to freshen local
    definitions allows us to be sloppy (and more concise) elsewhere. 
    
    The tradeoff to using this module is that variable names become
    longer (we alpha vary them by attaching unique suffixes); each pass
    through the module adds another unique suffix.  
    } *)

module Alpha_vary_locals : sig
  val of_program : Program.t -> Program.comb
end 
  =
struct
  module G  = Zipcfg
  module B  = G.Build
  module W  = G.Walk
  module P  = Program

  let ( **> ) f x = f x
    

  (* Variable substitution: maps old variables to new variables and
     performs substitutions over pures and lvalues.
  *)
  module Subst : sig 
    type t
    val empty      : t
    val extend     : t -> G.var  -> t
    val apply      : t -> G.var  -> G.var
    val apply_pure : t -> G.pure -> G.pure
    val apply_lval : t -> G.lval -> G.lval
  end 
    = 
  struct
    module M = Varinfo.Map
    type t = varinfo M.t
        
    let empty = M.empty
      
    let extend sub v = 
      let v' = Temps.fresh_ver v in
      M.add v v' sub
              
    let apply sub var =
      try M.find var sub 
      with Not_found -> var
        
    class visitor sub = object
      inherit nopCilVisitor
      method vvrbl (var:varinfo) = ChangeTo(apply sub var)
    end
      
    let apply_pure sub exp  = Cil.visitCilExpr (new visitor sub) exp
    let apply_lval sub lval = Cil.visitCilLval (new visitor sub) lval
  end    

    
  type subst = Subst.t
  let subst_extend     : subst -> G.var  -> subst  = Subst.extend
  let subst_apply      : subst -> G.var  -> G.var  = Subst.apply
  let subst_apply_lval : subst -> G.lval -> G.lval = Subst.apply_lval
  let subst_apply_pure : subst -> G.pure -> G.pure = Subst.apply_pure

  let subst_apply_simp : subst -> G.simp -> G.simp = 
    fun s -> function
      | G.Pure pure -> 
          G.Pure (subst_apply_pure s pure)
      
      | G.Read lval -> 
          G.Read (subst_apply_lval s lval)
      
      | G.Alloc typ -> 
          G.Alloc typ

      | G.Scope -> 
          G.Scope
      
      | G.Peek lval -> 
          G.Peek (subst_apply_lval s lval)

      | G.Poke (lval, pure) -> 
          G.Poke (subst_apply_lval s lval, subst_apply_pure s pure)

      | G.Low (op, pures) ->
          G.Low (op, (List.map (subst_apply_pure s) pures))

      | G.Clib (pure, pures) ->
          G.Clib (subst_apply_pure s pure, (List.map (subst_apply_pure s) pures))

      | G.Ffi (view, op, pures) ->
          G.Ffi (view, op, (List.map (subst_apply_pure s) pures))

  let subst_apply_scope : subst -> G.scope -> G.scope = 
    fun s -> function
      | G.Scope_change pure -> G.Scope_change (subst_apply_pure s pure)
      | G.Scope_same        -> G.Scope_same
    
  let subst_apply_memo : subst -> G.memo -> G.memo = 
    fun s -> function
      | G.Memo_yes (ppt, scope) -> G.Memo_yes (G.Ppt.from ppt, subst_apply_scope s scope)
      | G.Memo_no               -> G.Memo_no

  let subst_apply_br : subst -> G.branch -> G.branch = 
    fun s -> function 
      | G.Br (l, pures) -> G.Br (l, List.map (subst_apply_pure s) pures)
    
    
  type above = subst
  type below = G.zt

  let walk_graph =
    G.Walk.walk {
      
      G.Walk.walk_first = begin fun first subst ->
        match first with          
          | G.Entry -> subst, B.cons_id 
          
          | G.Label (l, ls, vars, ppt) ->
              let subst = List.fold_left subst_extend subst vars in 
              let vars  = List.map (subst_apply subst) vars in
              subst, B.cons_first (G.Label (l, ls, vars, G.Ppt.from ppt))
      end ;
      
      G.Walk.walk_middle = begin fun middle subst ->
        match middle with
          | G.Let_block (ls, ppt) -> 
              subst, B.let_blocks'' ppt ls

          | G.Update ppt ->
              subst, B.update ppt

          | G.Let_simp (var, simp, ppt) ->
              let subst = 
                if var = Temps.wildcard_var 
                then subst 
                else subst_extend subst var 
              in
              subst, B.let_simp ppt
                (subst_apply      subst var)
                (subst_apply_simp subst simp)

          | G.Cut (label, memo, ppt) ->
              subst, B.cut ppt label 
                (subst_apply_memo subst memo)

          | G.Write (lval, pure, ppt) ->
              subst, B.write ppt
                (subst_apply_lval subst lval)
                (subst_apply_pure subst pure)

          | G.Memo (scope, ppt) ->
              subst, B.memo ppt
                (subst_apply_scope subst scope)

          | G.Rvars (rvars, ppt) ->
              subst, B.rvars ppt
                (List.map (subst_apply subst) rvars)
      end ;
      
      G.Walk.walk_last = begin fun last subst ->
        match last with
          | G.Exit -> G.emptyz
          
          | G.Cond (pure, br1, br2, ppt) ->
              B.cond ppt
                (subst_apply_pure subst pure)
                (subst_apply_br   subst br1)
                (subst_apply_br   subst br2)
                G.emptyz

          | G.Branch (br, ppt) ->
              B.branch ppt
                (subst_apply_br subst br)
                G.emptyz

          | G.Call (mode, pure, pures, ppt) ->
              B.call ppt mode
                (subst_apply_pure subst pure)
                (List.map (subst_apply_pure subst) pures)
                G.emptyz
                
          | G.Return (pures, ppt) ->
              B.return ppt
                (List.map (subst_apply_pure subst) pures)
                G.emptyz
      end ;
      
      G.Walk.walk_nested = begin fun labels subst ->
        subst, G.Copy.insert_nested 
      end ;
    
    }       

  let of_program (program:P.t) : P.comb = 
    P.Walk.walk {
      
      P.Copy.walk_fns with
        
        P.Walk.walk_func = begin fun (var, args, graph, ppt) a ->
          let subst = List.fold_left subst_extend Subst.empty args in
          let args  = List.map (subst_apply subst) args in
          a, fun comb z -> P.Build.func'' ppt
            begin var, args, 
              G.unfocus **> walk_graph subst (G.entry graph), 
              G.Ppt.from ppt
            end **> comb **> z
        end ;

    } () program
          

end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(** {3 General-purpose simplification; run after each pass } *)

module Simplify : sig
  val of_program : Program.t -> Program.comb
end
  = 
struct
  module G  = Zipcfg
  module B  = G.Build
  module W  = G.Walk
  module P  = Program

  let ( **> ) f x = f x

  (* We take as an invariant that the scope of Let_blocks reflects the
     dominator tree for a given graph.  This module renests the scope of
     Let_blocks so that this invariant holds.  Sometimes transformations
     will disrupt this correspondance (e.g., the DPS-conversion does so);
     rather than try to preserve the invariant throughout every
     transformation, we use this module to restore it. *)
  module Redominate = struct
    
    module D = Dom_summary

    let counter = ref 0

    let rec of_graph graphz =
      
      let dom = D.of_graph false (None) (G.unfocus graphz) in
      
      let label_of_ppt : G.ppt -> G.label option = 
        G.label_of_ppt (G.unfocus graphz) in
      
      let let_blocks_for_ppt ppt =
        (*
          Printf.printf "%s : (%s --> %s) \n" (Ppt.string pt) (Ppt.string (D.imm_dom dom pt)) (Ppt.string pt);
          List.iter (fun kid ->
          Printf.printf "%s --> %s\n" (Ppt.string pt) (Ppt.string kid))
          (D.dom_kids dom pt)
          ;
          Printf.printf "- - - - \n" ;
        *)
        (* We let-bind blocks for program points that we dominate, but
           only when these program points are labeled in the
           program. *)
        B.let_blocks'' ppt
          ( List.fold_right (function 
                               | Some label -> (fun labels -> label :: labels)
                               | None       -> (fun labels ->          labels))                  
              ( List.map label_of_ppt (D.dom_kids dom ppt) ) []
          )
      in
      W.walk 
        { G.Copy.walk_fns with
            
            (* W.walk_first = begin fun f above ->
              
              (
                match  G.pp_of_first f with
                  | Some pt -> ignore ( let_blocks_for_pt pt G.emptyz )
                  | None    -> () 
              ) ;
              above, B.cons_first f
            end ; *)

            W.walk_middle = begin fun m above -> 
              let ppt = G.ppt_of_middle m in
              above, fun z -> 
                begin let_blocks_for_ppt ppt
                  **> ( match m with
                          | G.Let_block _ -> B.cons_id (* erase the old Let_blocks *)
                          | _             -> B.cons_middle m
                      )
                  **> z end
            end ;
            
            W.walk_last = begin fun last above ->
              ( match G.ppt_of_last last with 
                  | None     -> B.cons_id 
                  | Some ppt -> let_blocks_for_ppt ppt
              )
                (B.cons_last last G.emptyz) 
            end ;

        } () graphz
  end

  module Contract_blocks = struct
    
    let rec of_graph graphz = 
      let graph = G.unfocus graphz in
      
      (* True iff the given basic block will be contracted into it's
         (unique) predecessor *or* if the block has zero predecessors
         (in which case it can be dropped).  *)
      let is_superfluous_label : label -> bool =
        let preds_of, succs_of =
          let graph_preds = Predecessors.of_graph false graph in
          (Predecessors.preds_of graph_preds,
           Predecessors.succs_of graph_preds)
        in
        let preds_of ppt = try (preds_of ppt) with Not_found -> [] in
        begin fun label ->
          let label_ppt = G.ppt_of_label label graph in
          match preds_of label_ppt with
            | [pred_ppt] -> succs_of pred_ppt = [label_ppt] 
            | []        -> true
            | _         -> false
        end
      in
      W.walk 
        { G.Copy.walk_fns with
            W.walk_last = begin function
              | (G.Branch ((G.Br (label, pures)), ppt)) as last-> begin 
                  if is_superfluous_label label then fun _ ->
                    (* branch is only predecessor; inline the block. *)
                    let (_,_,vars,_) = G.info_of_label label graph in
                    List.fold_right2 begin fun var pure z ->
                      B.let_simp ppt var (G.Pure pure) z
                    end vars pures 
                      (* When inlining the block, its crucial that we
                       * don't just copy it from the old graph; rather,
                       * we must use apply a consistent set of
                       * simplifications to it; failing to do this can
                       * result in dangling branches. *)
                    **> (G.drop_head (of_graph (G.focus label graph)))
                  else 
                    (* retain the branch. *)
                    G.Copy.walk_fns.W.walk_last last
                end

              | last -> G.Copy.walk_fns.W.walk_last last
            end ;          
            
            W.walk_nested = begin fun labels a -> a, fun zs z ->            
              let zs_to_keep = List.fold_right2 
                begin fun label z keep ->
                  if is_superfluous_label label 
                  then keep 
                  else z :: keep
                end labels zs [] in
              G.Copy.insert_nested zs_to_keep z
            end ;

            W.walk_middle = begin function
              | G.Let_block (labels, ppt) -> fun a -> a,
                  B.let_blocks'' ppt begin
                    List.filter begin fun label -> not 
                      (is_superfluous_label label)
                    end labels
                  end
                | middle -> G.Copy.walk_fns.W.walk_middle middle
            end ;
        } () graphz
  end (* Contract_blocks module *)

  let of_graph (graphz:G.zt) : G.zt =    
    begin Redominate.of_graph      (* reposition the scoping of blocks to respect dominator tree. *)
      **> Contract_blocks.of_graph (* do block-inlining *)
      **> Lift_returns.of_graph    (* duplicate/lift returns *)
      **> graphz
    end

  let of_program (prog:P.t) : P.comb =
    let walk_fns : (unit, P.comb) P.Walk.walk_fns = {      
      P.Copy.walk_fns with 
        P.Walk.walk_func = begin 
          fun (fun_var, fun_vars, fun_graph, fun_pt) a -> a, 
            fun comb frag ->
              P.Build.func'' fun_pt begin
                fun_var, fun_vars,
                G.unfocus (of_graph (G.entry fun_graph)),
                G.Ppt.from fun_pt
              end **> comb **> frag
        end ; }
    in
    P.Walk.walk walk_fns () prog
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Instruments programs and graphs with tv signals.  
   See ../src/tv/tv_signal.atd.
*)
module Tv_signals = struct
  module G  = Zipcfg
  module B  = G.Build
  module W  = G.Walk
  module P  = Program
  module L  = Low

  let ( **> ) f x = f x
    
  (* TODO: Put these into a Common module *)
  let offs_of_fld  fld  = Cil.Field (fld, Cil.NoOffset)
  let lval_of_var  v    = Cil.Var v, Cil.NoOffset
  let pure_of_var  v    = Cil.Lval (lval_of_var v)
  let pure_of_lval lval = Cil.Lval lval

  let format_string_of_type t = 
    match Cil.typeSig t with
      | Cil.TSPtr _ -> "%p"
      | Cil.TSBase t -> begin match t with
          | Cil.TInt(kind, _)   -> begin match kind with
              | Cil.IChar       -> "%d"
              | Cil.ISChar      -> "%d"
              | Cil.IUChar      -> "%d"
              | Cil.IBool       -> "%d"
              | Cil.IInt        -> "%d"
              | Cil.IUInt       -> "%d"
              | Cil.IShort      -> "%d"
              | Cil.IUShort     -> "%d"
              | Cil.ILong       -> "%ld"
              | Cil.IULong      -> "%ld"
              | Cil.ILongLong   -> "%lld"
              | Cil.IULongLong  -> "%lld"
            end
              
          | Cil.TFloat(kind, _) -> begin match kind with
              | Cil.FFloat      -> "%g"
              | Cil.FDouble     -> "%g"
              | Cil.FLongDouble -> "%g"
            end
              
          | _ -> raise Not_found
        end
      | Cil.TSEnum _ -> "%d"
      | _ -> raise Not_found

  let wc_var = Temps.wildcard_var

  let exp_of_bool     = fun b -> if b then Cil.one else Cil.zero 
  let exp_of_int i    = Cil.integer i
  let null            = Cil.mkCast ~e:Cil.zero ~newt:Cil.voidPtrType 

  let exp_of_var v    = Cil.Lval(Cil.Var v, Cil.NoOffset)
  let exp_of_string s = Cil.Const (Cil.CStr s) 
  let exp_of_qual q   = exp_of_string (Qual.string_of_qual q)
  let exp_of_typ t    = exp_of_string (string_of_type t)
    
  let fmtexp_of_typ t = exp_of_string ("\"" ^ (format_string_of_type t) ^ "\"")


  let tvsig ppt op_name args = 
    B.let_simp ppt wc_var (G.Low((L.Tv_signal op_name), args))
    
  let tvsig_meta_begin ppt = 
    tvsig ppt "meta_begin"
      [ exp_of_int    (G.Ppt.id   ppt) ;
        exp_of_string (G.Ppt.file ppt) ;
        exp_of_string (match G.Ppt.fname ppt with Some s -> s | None -> "") ;
        exp_of_int    (G.Ppt.line ppt) ;
        exp_of_int    (G.Ppt.byte ppt) ]

  let tvsig_meta_end ppt = tvsig ppt "meta_end" []

  let ( 
    tvsig_invoke_begin,
    tvsig_invoke_end,
    tvsig_revinv_begin,
    tvsig_revinv_end ) 
      =    
    let tvsig_begin op_name ppt trnode handle = 
      tvsig ppt op_name 
        [ exp_of_int    (G.Ppt.id    ppt) ;
          exp_of_string (G.Ppt.file  ppt) ;
          exp_of_string (match G.Ppt.fname ppt with Some s -> s | None -> "") ;
          exp_of_int    (G.Ppt.line  ppt) ;
          exp_of_int    (G.Ppt.byte  ppt) ;
          trnode ; handle ; 
          (* size of handle, as a payload; 
             we take the size of the handle pointer when dereferenced. *)
          Cil.sizeOf (Cil.typeOf (Cil.Lval(Cil.Mem handle, Cil.NoOffset)))
        ] 
    in
    let tvsig_end op_name ppt = 
      tvsig ppt op_name []
    in    
    ( tvsig_begin "invoke_begin" ,
      tvsig_end   "invoke_end" ,
      tvsig_begin "revinv_begin" ,
      tvsig_end   "revinv_end" )
      
  let tvsig_revoke ppt trnode handle = 
    tvsig ppt "revoke" [trnode; handle]

  exception Not_implemented of string * int

  let tvsig_vals ppt pures = fun z ->
    begin tvsig ppt "vals_begin" [] **> 
        List.fold_right begin fun pure ->
          tvsig ppt "vals_val" [ exp_of_typ (Cil.typeOf pure);
                                 fmtexp_of_typ (Cil.typeOf pure);
                                 pure ] 
        end pures
      **> tvsig ppt "vals_end" []
      **> z
    end
        
  let tvsig_env ppt vars (quasi_vars : (string * G.pure) list)  = fun z ->
    begin tvsig ppt "env_begin" [] **>
        List.fold_right begin fun (name,pure) ->
          tvsig ppt "env_var" [ exp_of_string name ;
                                exp_of_string (string_of_type (Cil.typeOf pure));
                                fmtexp_of_typ (Cil.typeOf pure);
                                pure ]
        end quasi_vars
      **>
        List.fold_right begin fun var ->
          tvsig ppt "env_var" [ exp_of_string (var.Cil.vname);
                                exp_of_string (string_of_type (Cil.typeOf (pure_of_var var)));
                                fmtexp_of_typ (Cil.typeOf (pure_of_var var));
                                (pure_of_var var) ]
        end vars
      **> tvsig ppt "env_end" []
      **> z
    end
  
  (* Give me Low.op info, then give me a zipper, then i give you code. *)
  let tvsig_low_op = fun (level:[`Meta|`Core]) -> 
    fun ppt var low_op args -> fun z -> match (level, low_op, args) with
    
      | `Meta, L.Init, []          -> z
      
      | `Meta, L.Core_begin, [] -> 
          begin tvsig_meta_begin ppt 
            **> tvsig ppt "m_core_begin" []
            **> tvsig_meta_end ppt 
            **> z 
          end

      | `Meta, L.Core_end, [] -> 
          begin tvsig_meta_begin ppt
            **> tvsig ppt "m_core_end" []
            **> tvsig_meta_end ppt
            **> z
          end

      | `Meta, L.Alloc_kill, [ptr] -> 
          begin tvsig_meta_begin ppt 
            **> tvsig ppt "m_kill" [ptr]
            **> tvsig_meta_end ppt
            **> z
          end
      
      | `Meta, L.Alloc_invoke, [trnode;alloch;size] -> 
          begin tvsig_meta_begin ppt
            **> tvsig ppt "m_alloc" [ exp_of_typ var.Cil.vtype ; size ; exp_of_var var ]
            **> tvsig_meta_end ppt
            **> z
          end
          
      | `Meta, L.Read_invoke (q,typ), [_;_;ptr] ->
          begin tvsig_meta_begin ppt
            **> tvsig ppt "m_read" [ 
              exp_of_qual q ; exp_of_typ typ ;
              ptr ; fmtexp_of_typ typ ; exp_of_var var ]
            **> tvsig_meta_end ppt
            **> z
            end
          
      | `Meta, L.Write_invoke (q,typ), [_;_;ptr;value] ->
          begin tvsig_meta_begin ppt
            **> tvsig ppt "m_write" [ 
              exp_of_qual q ; exp_of_typ typ ;
              ptr ; fmtexp_of_typ typ ; value ]
            **> tvsig_meta_end ppt
            **> z
          end

      | `Meta, L.Propagate, [] -> failwith "Low.Propagate not handled here."
        
      | `Core, L.Trace_node, [descr] -> z

      | `Core, L.Cut_begin, [] -> 
          begin tvsig_invoke_begin ppt null null
            **> tvsig ppt "push_begin" [] 
            **> tvsig_invoke_end ppt
            **> z
          end
            
      | `Core, L.Cut_end, [] ->
          begin tvsig_invoke_begin ppt null null
            **> tvsig ppt "push_end" [] 
            **> tvsig_invoke_end ppt
            **> z
          end

      | `Core, L.Update_invoke vars, [trnode;updateh] ->
          begin tvsig_invoke_begin ppt trnode updateh
            **> tvsig ppt "update_begin" []
            **> tvsig_env ppt vars []
            **> tvsig ppt "update_end" []
            **> tvsig_invoke_end ppt
            **> z
          end
          
      | `Core, L.Memo_invoke vars, [trnode;memoh;memot;bytes;bytec] -> 
          let cont_label   = Temps.fresh_label (G.Ppt.loc ppt) in
          let signal_label = Temps.fresh_label (G.Ppt.loc ppt) in
          begin B.let_block ppt cont_label [] 
            ( fun _ -> z )
            **> B.let_block ppt signal_label [] 
            ( fun z -> 
                begin tvsig_invoke_begin ppt trnode memoh
                  **> tvsig ppt "memo_begin" []
                  **> tvsig_env ppt vars [("$assign_scope", memot)]
                  **> tvsig ppt "memo_end" []
                  **> tvsig_invoke_end ppt
                  **> B.branch ppt (G.Br (cont_label, []))
                  **> z end )
            **> B.cond ppt (exp_of_var var)
              (G.Br (cont_label, []))
              (G.Br (signal_label, []))            
            **> G.emptyz
          end
            
      | `Core, ( L.Alloc_invoke | L.Unboxed_invoke ), [trnode;alloch;size] -> 
          begin tvsig_invoke_begin ppt trnode alloch
            **> tvsig ppt "alloc" [ 
              exp_of_typ var.Cil.vtype ; size ; exp_of_var var ]
            **> tvsig_invoke_end ppt
            **> z
          end
            
      | `Core, ( L.Alloc_revinv | L.Unboxed_revinv ), [trnode;alloch;size] ->
          begin tvsig_revinv_begin ppt trnode alloch
            **> tvsig ppt "alloc" [ 
              exp_of_typ var.Cil.vtype ; size ; exp_of_var var ]
            **> tvsig_revinv_end ppt
            **> z
          end

      | `Core, L.Scope_invoke, [trnode;scopeh] -> 
          begin tvsig_invoke_begin ppt trnode scopeh
            **> tvsig ppt "scope" [ exp_of_var var ]
            **> tvsig_invoke_end ppt
            **> z
          end

      | `Core, L.Scope_revinv, [trnode;scopeh] ->
          begin tvsig_revinv_begin ppt trnode scopeh
            **> tvsig ppt "scope" [ exp_of_var var ]
            **> tvsig_revinv_end ppt
            **> z
          end
                
      | `Core, L.Read_invoke (q,typ), [trnode;readh;ptr] ->
          begin tvsig_invoke_begin ppt trnode readh
            **> tvsig ppt "read" [ 
              exp_of_qual q ; exp_of_typ typ ;
              ptr ; fmtexp_of_typ typ ; exp_of_var var ]
            **> tvsig_invoke_end ppt
            **> z
          end
            
      | `Core, L.Read_revinv (q,typ), [trnode;readh;ptr] ->
          begin tvsig_revinv_begin ppt trnode readh
            **> tvsig ppt "read" [ 
              exp_of_qual q ; exp_of_typ typ ;
              ptr ; fmtexp_of_typ typ ; exp_of_var var ]
            **> tvsig_revinv_end ppt
            **> z
          end
            
    | `Core, L.Write_invoke (q,typ), [trnode;writeh;ptr;value] ->
        begin tvsig_invoke_begin ppt trnode writeh
          **> tvsig ppt "write" [ 
            exp_of_qual q ; exp_of_typ typ ;
            ptr ; fmtexp_of_typ typ ; value ]
          **> tvsig_invoke_end ppt
          **> z
        end
          
    | `Core, L.Write_revinv (q,typ), [trnode;writeh;ptr;value] ->
        begin tvsig_revinv_begin ppt trnode writeh
          **> tvsig ppt "write" [ 
            exp_of_qual q; exp_of_typ typ ;
            ptr ; fmtexp_of_typ typ ; value ]
          **> tvsig_revinv_end ppt
          **> z
        end
          
    | _, L.Tv_signal _, _ -> failwith "Shouldn't be here."
          
    | `Core, 
        ( L.Memo_revoke
        | L.Update_revoke
        | L.Unboxed_revoke
        | L.Alloc_revoke
        | L.Scope_revoke
        | L.Read_revoke  _
        | L.Write_revoke _ ), trnode :: handle :: _
          -> begin tvsig_revoke ppt trnode handle **> z end

    | _, op, args -> raise (Not_implemented (Low.name_of_op op, List.length args))
        
    (* Nothing matches.  Eventually this shouldn't be possible. *)
        
  let of_graph (level:[`Meta|`Core]) (graphz:G.zt) : G.zt =
    W.walk { 
      G.Copy.walk_fns with
        W.walk_middle = begin function
          | (G.Let_simp (v, G.Low (low_op, args), ppt)) as m -> 
              begin fun a -> a, match low_op with
                | L.Propagate -> ( fun z -> 
                                     begin tvsig ppt "propagate_begin" []
                                       **> B.cons_middle m
                                       **> tvsig ppt "propagate_end" []
                                       **> z
                                     end )
                    
                | _ -> (fun z -> 
                          begin B.cons_middle m 
                            **> tvsig_low_op level ppt v low_op args 
                            **> z
                          end)
              end

          | middle -> G.Copy.walk_fns.W.walk_middle middle
        end ;
        
        W.walk_last = begin function 
          | (G.Call (_, pure, pures, ppt)) as last when level = `Core -> begin fun a ->
              begin tvsig_invoke_begin ppt null null
                **> tvsig ppt "tcall_begin" [exp_of_string (string_of_exp pure)]
                **> tvsig_vals ppt pures
                **> tvsig ppt "tcall_end" []
                **> tvsig_invoke_end ppt
                **> B.cons_last last
                **> G.emptyz
              end
            end
            
          | (G.Return (pures, ppt)) as last 
              when level = `Core 
                && not (G.Property.has (G.Property.Undo_code) ppt)
                && not (G.Property.has (G.Property.Memo_code) ppt) -> 
              begin fun a ->
                begin tvsig_invoke_begin ppt null null
                  **> tvsig ppt "pop_begin" []
                  **> tvsig_vals ppt pures
                  **> tvsig ppt "pop_end" []
                  **> tvsig_invoke_end ppt
                  **> B.cons_last last
                  **> G.emptyz
                end
              end
   
        | last -> G.Copy.walk_fns.W.walk_last last
        end ;
        
    } () graphz

  let of_program (level:[`Meta|`Core]) (prog:P.t) : P.comb =
    let walk_fns : (unit, P.comb) P.Walk.walk_fns = {      
      P.Copy.walk_fns with 
        P.Walk.walk_func = begin 
          fun (fun_var, fun_vars, fun_graph, fun_pt) a -> a, 
            fun comb frag ->
              let graph = of_graph level (G.entry fun_graph) in
              (* We were putting this initialization call in the code;
                 now it's apart of the ceal_init runtime call (See
                 Low.Init). *)
              (*
                let graph = 
                if fun_var.Cil.vname = "main" && level = `Meta then
                ( B.let_simp fun_pt Temps.wildcard_var 
                (G.Low (L.Tv_signal "init", []))
                **> graph )
                else
                graph
              in
              *)
              P.Build.func'' fun_pt begin
                fun_var, fun_vars, G.unfocus graph, G.Ppt.from fun_pt
              end 
              **> comb **> frag
        end ; }
    in
    P.Walk.walk walk_fns () prog
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
module To_cil = struct
  module G  = Zipcfg
  module P  = Program

  let lval_of_var   var = (Cil.Var var, Cil.NoOffset)
  let pure_of_lval lval = Cil.Lval lval
  let pure_of_var   var = pure_of_lval (lval_of_var var)
    
  (* At this point, it's incorrect for blocks to end with Exit *)
  exception Last_is_exit
  exception Last_is_exit_in_func of string

  (* - - - - - - - - - - *)

  (* Elimating cuts: We use a stack of return-labels to transform
     cut-returns into assignments & gotos. *)
  type stack = G.label list

  (* From zipcfg to Cil: We map each subtree of our Zipcfg (defined
     syntantically) into a cil_tree: *)
  type cil_tree = {
    stmts  : Cil.stmt    list ;
    instrs : Cil.instr   list ;    
    vars   : Cil.varinfo list -> Cil.varinfo list ;
    blocks : Cil.block   list ->   Cil.block list ;
  }

  let cil_tree_empty = { 
    stmts  = [] ;
    instrs = [] ;
    vars   = (fun vs -> vs) ;
    blocks = (fun bs -> bs) }      
    
  type cil_comb = cil_tree -> cil_tree
    
  type cil_hole =
      [ `Tempvars of Cil.varinfo list
      | `Vardecl  of Cil.varinfo
      | `Label    of Cil.label * Cil.varinfo list
      | `Instr    of Cil.instr 
      | `Stmtkind of Cil.stmtkind
      | `Stmts    of Cil.stmt list
      ]

  (* Change instructions into a statement *)
  let cil_tree_no_instrs : cil_comb = fun ct ->
    let stmts = match ct.instrs with
      | []     -> ct.stmts
      | instrs -> Cil.mkStmt (Instr instrs) :: ct.stmts
    in
    { ct with instrs = [] ; stmts = stmts }    
      
  (* Turn a cil_tree into a Cil.block. *)
  let cil_block_of_cil_tree (ct : cil_tree) : Cil.block = 
    let ct = cil_tree_no_instrs ct in
    let stmts = ct.stmts @ begin List.map 
        (fun b -> Cil.mkStmt (Cil.Block b)) 
        (ct.blocks []) end 
    in
    Cil.mkBlock stmts 

  (* Insert stuff into a cil_tree *)
  let cil_insert (stmt_of_label : label -> Cil.stmt) 
      : cil_hole -> cil_comb 
    = function
      | `Tempvars vars -> begin fun ct ->
          { ct with vars = (fun vs -> vars @ (ct.vars vs)) }
        end

      | `Vardecl var -> begin fun ct ->
          if var <> Temps.wildcard_var then
            { ct with vars = (fun vs -> var :: (ct.vars vs)) }
          else ct
        end
          
      | `Instr instr -> begin fun ct -> 
          { ct with instrs = instr :: ct.instrs } 
        end
          
      | `Stmtkind stmtkind -> begin fun ct -> 
          let ct = cil_tree_no_instrs ct in
          { ct with stmts = (Cil.mkStmt stmtkind) :: ct.stmts }
        end
          
      | `Stmts stmts -> begin fun ct ->
          let ct = cil_tree_no_instrs ct in
          { ct with stmts = stmts @ ct.stmts }
        end
          
      | `Label (label, vars) -> begin fun ct ->
          let stmt  = stmt_of_label label in
          let ct    = cil_tree_no_instrs ct in
          let block = Cil.mkBlock (stmt :: ct.stmts) in
          { cil_tree_empty with 
              vars   = (fun vs -> vars  @  (ct.vars vs)) ;
              blocks = (fun bs -> block :: (ct.blocks bs)) }
        end

  let walk_simp 
      (stmt_of_label : label -> Cil.stmt) 
      (ppt:G.ppt) (lval:Cil.lval) 
      = 
    let lval_opt = 
      (* The Cil.Call constructor requires an lval option. *)
      match lval with 
        | (Cil.Var wc, Cil.NoOffset) when wc == Temps.wildcard_var -> None
        | _ -> Some lval
    in
    let cil_insert = cil_insert stmt_of_label in 
    function
      | G.Pure pure -> 
        cil_insert (`Instr (Cil.Set (lval, pure, G.Ppt.loc ppt)))
          
      | G.Peek (lval_src) ->
          cil_insert (`Instr (Cil.Set (lval, pure_of_lval lval_src, G.Ppt.loc ppt)))
            
      | G.Poke (lval_dst, pure) ->
          cil_insert (`Instr (Cil.Set (lval_dst, pure, G.Ppt.loc ppt)))
            
      | G.Low (low_op, pures) ->
          let low_exp = (pure_of_var (Low.var_of_op ppt low_op)) in
          cil_insert (`Instr (Cil.Call (lval_opt, low_exp, pures, G.Ppt.loc ppt)))

      | G.Ffi (view, op, args) ->
          let fvar = Ffi.resolve op view in
          cil_insert (`Instr (Cil.Call (lval_opt, pure_of_var fvar, args, G.Ppt.loc ppt)))
          
      | G.Clib (pure, pures) ->
          cil_insert (`Instr (Cil.Call (lval_opt, pure, pures, G.Ppt.loc ppt)))

      | simp -> 
          (* TODO: This is an internal error, but perhaps a less
             cryptic error message would be nice. *)
          Pretty.fprint stderr ~width:80 
            (Pretty.dprintf "Error: simp `%a' is illegal here.@!"
               G.Print.pr_simp_doc simp) ;
          assert false

  (* - - - - - - - - - - *)
          
  let graph_walk_fns 
      (return_type   : Cil.typ)
      (stmt_of_label : label -> Cil.stmt)
      (graph : G.t) 
      : (stack, cil_tree) G.Walk.walk_fns =
    
    let cil_insert = cil_insert stmt_of_label in

    {
      G.Walk.walk_nested = begin fun _ a -> a,
        List.fold_right begin fun ct' ct ->
          assert ( ct'.stmts  = [] ) ;
          assert ( ct'.instrs = [] ) ;
          { ct with 
              vars   = (fun vs -> ct.vars   (ct'.vars   vs)) ;
              blocks = (fun bs -> ct.blocks (ct'.blocks bs)) }          
        end
      end (* walk_nested *) ;

      G.Walk.walk_first = begin function
        | G.Entry -> (fun a -> a, fun b -> b)

        | G.Label (label, labels, vars, pt) -> begin
            fun a -> a, cil_insert (`Label (label, vars))
          end
      
      end (* walk_first *) ;
      
      G.Walk.walk_middle = begin function          
        (* Memo forms should already be eliminated *)
        | G.Memo _      -> assert false
        | G.Update _    -> assert false        
        | G.Rvars _     -> assert false

        (* We handle nested blocks in walk_nested *)
        | G.Let_block _ -> begin fun a -> a, fun b -> b end
            
        (* We eliminate this by converting back to direct style *)
        | G.Cut (label, _, ppt) -> begin 
            fun stack -> label :: stack, fun b -> b
          end

        (* Writes are ordinary instructions (they only affect "stable" memory) *)
        (* TODO -- Should we make G.Write another case of the simp type? *)
        | G.Write (lval, pure, ppt) -> begin fun a -> a,
            cil_insert (`Instr (Cil.Set (lval, pure, G.Ppt.loc ppt)))
          end
            
        (* We handle simple bindings using 1 instruction each. *)
        | G.Let_simp (var, simp, ppt) -> 
            let comb   = fun ct -> 
              cil_insert (`Vardecl var)
              **> walk_simp stmt_of_label ppt (lval_of_var var) simp 
              **> ct
            in
            (fun a -> a, comb)
            
      end (* walk_middle *);
      
      G.Walk.walk_last = begin
        let stmts_of_br ppt = function
          | G.Br (label, pures) ->
              (* Note: We need to do the assignments below _simultaneously_.  
                 Hence, we need temps. *)
              let formals = G.formals_of_label label graph in

              (* Create some temporary variables -- one for each formal. *)
              let temps = List.map Temps.fresh_ver formals in
              
              (* Temps get assigned from actual args *)
              let temps_get_actuals =
                List.map2 begin fun temp pure ->
                  Cil.Set (lval_of_var temp, pure, G.Ppt.loc ppt) 
                end temps pures
              in

              (* Formal args get assigned from temps *)
              let formals_get_temps =
                List.map2 begin fun formal temp ->
                  Cil.Set (lval_of_var formal, pure_of_var temp, G.Ppt.loc ppt)
                end formals temps
              in

              (* Assign args; Goto the statement with the given label. *)
              temps,
              [ Cil.mkStmt (Cil.Instr ( temps_get_actuals @ formals_get_temps)) ;
                Cil.mkStmt (Cil.Goto (ref (stmt_of_label label), G.Ppt.loc ppt)) ]
        in
        
        let cil_insert_br ppt br =
          let vars, stmts = stmts_of_br ppt br in
          fun ct -> 
            begin cil_insert (`Tempvars vars) 
              **> cil_insert (`Stmts stmts)
              **> ct
            end
        in

        function
          | G.Exit -> raise Last_is_exit
              (* (fun _ -> cil_tree_empty) *)
              
          | G.Return (pures, ppt) -> begin fun stack -> 
              match stack, pures with
                  
                (* End of cut body -- returns zero or more values *)
                | label :: _, pures -> 
                    cil_insert_br ppt (G.Br (label, pures))
                      (* cil_insert (`Stmts (stmts_of_br ppt (G.Br (label, pures)))) *)
                      cil_tree_empty

                (* End of function -- returns no value *)
                | [], [] ->
                    cil_insert (`Stmtkind (Cil.Return (None, G.Ppt.loc ppt)))
                      cil_tree_empty
                    
                (* End of function -- returns a single value *)
                | [], [pure] -> 
                    cil_insert (`Stmtkind (Cil.Return (Some pure, G.Ppt.loc ppt)))
                      cil_tree_empty

                (* End of function -- returns several values *)
                | [], pures ->
                    let fields = match return_type with 
                      | Cil.TComp (ci, _) -> ci.Cil.cfields 
                      | _                 -> assert false
                    in                    
                    (* Create a temporary (composite) variable to hold
                       the return values *)
                    let rvs = Temps.fresh_var return_type in                    
                    cil_insert (`Vardecl rvs) **>

                    (* Set the fields of the return variable *)
                    List.fold_right2 begin fun field pure ->
                      let lval = (Cil.Var rvs, Cil.Field (field, Cil.NoOffset)) in
                      cil_insert (`Instr (Cil.Set (lval, pure, G.Ppt.loc ppt))) 
                    end fields pures                      
                      
                      (* Return the temporary variable *)
                      (let rvs_as_exp = Cil.Lval (Cil.Var rvs, Cil.NoOffset) in
                       cil_insert (`Stmtkind (Cil.Return (Some rvs_as_exp, G.Ppt.loc ppt)))
                       **> cil_tree_empty)
            end
                      
          | G.Call (mode, pure, pures, ppt) -> begin fun stack -> 
              let call_return_type : G.typ = 
                (* This is the return type of the call.  In the case
                   that the call is a tail-call, it should match the
                   return_type for the current function; otherwise, it
                   need not match and we use it below in the non-tail
                   case to create a temporary variable for the
                   result. *)
                match Cil.typeOf pure with
                  | Cil.TFun (t, _, _, _) -> t
                  | _                     -> assert false
              in
              
              (*
              ignore (log "call to: %s\n" (string_of_exp pure)) ;
              ignore (log "  return type: %s\n" (string_of_type return_type)) ;
              *)

              match mode, stack, (Cil.typeSig call_return_type) with
                (* Invalid usage of Run_core -- 
                   should have been 'lowered away' before using this pass. *)
                | G.Run_core, _, _ -> assert false 

                (* Non-tail call (Call ends a cut body).
                   No return value (Call returns void). *)
                | G.Default, label :: _, (Cil.TSBase (Cil.TVoid _)) ->
                    cil_insert (`Instr (Cil.Call (None, pure, pures, G.Ppt.loc ppt)))
                    **> cil_insert_br ppt (G.Br (label, []))
                    (* **> cil_insert (`Stmts (stmts_of_br pt (G.Br (label, [])))) *)
                      cil_tree_empty

                (* Non-tail call (Call ends a cut body).
                   Has (one or more) return values. *)
                | G.Default, label :: _, _ ->
                    let rvs         : G.var       = Temps.fresh_var call_return_type in
                    let rvs_lval    : G.lval      = (Cil.Var rvs, Cil.NoOffset) in
                    let rvs_pures   : G.pure list =
                      List.map pure_of_lval begin 
                        match call_return_type with 
                          | Cil.TComp (ci, _) -> 
                              List.map begin fun field ->
                                (Cil.Var rvs, Cil.Field (field, Cil.NoOffset))
                              end ci.Cil.cfields                     
                          | _ -> [(Cil.Var rvs, Cil.NoOffset)]
                      end
                    in
                    cil_insert (`Vardecl rvs)
                    **> cil_insert (`Instr (Cil.Call (Some rvs_lval, pure, pures, G.Ppt.loc ppt)))
                    (* **> cil_insert (`Stmts (stmts_of_br ppt (G.Br (label, rvs_pures)))) *)
                    **> cil_insert_br ppt (G.Br (label, rvs_pures))
                    **> cil_tree_empty
                    
                (* Tail-call (Call ends function body) -- 
                   No return value (Call returns void). *)
                | G.Default, [], (Cil.TSBase (Cil.TVoid _)) ->
                    cil_insert (`Instr (Cil.Call (None, pure, pures, G.Ppt.loc ppt)))
                    **> cil_insert (`Stmtkind (Cil.Return (None, G.Ppt.loc ppt)))
                    **> cil_tree_empty

                (* Tail-call (Call ends function body) -- 
                   Has (one or more) return values. *)
                | G.Default, [], _ ->
                    let rvs      = Temps.fresh_var call_return_type in
                    let rvs_lval = (Cil.Var rvs, Cil.NoOffset) in
                    (* BUG-FIX: we do a tricky cast here to allow for
                       nominally-distinct-yet-structually-equal C structs. *)
                    (* The idea is that the return type of the callee
                       will not be nominally equal with the return
                       type of the caller; we cannot do a cast
                       directly, as these types are encoded as C
                       structs; however, by adding some pointer
                       indirection on either side of the cast, C is
                       happy. *)
                    let rvs_pure = 
                      let casted_struct_ptr = 
                        Cil.CastE( Cil.TPtr(return_type,[]),
                                   Cil.AddrOf rvs_lval )
                      in
                      Cil.Lval(Cil.Mem(casted_struct_ptr), Cil.NoOffset)
                    in
                    cil_insert (`Vardecl rvs)
                    **> cil_insert (`Instr (Cil.Call (Some rvs_lval, pure, pures, G.Ppt.loc ppt)))
                    **> cil_insert (`Stmtkind (Cil.Return (Some rvs_pure, G.Ppt.loc ppt)))
                    **> cil_tree_empty
            end
              
          | G.Cond (pure, br1, br2, ppt) -> begin fun _ ->
              let vars1, stmts1 = (stmts_of_br ppt br1) in
              let vars2, stmts2 = (stmts_of_br ppt br2) in
              let b1 = Cil.mkBlock stmts1 in
              let b2 = Cil.mkBlock stmts2 in
              begin cil_insert (`Tempvars vars1)
                **> cil_insert (`Tempvars vars2)
                **> cil_insert (`Stmtkind (Cil.If(pure, b1, b2, G.Ppt.loc ppt)))
                **> cil_tree_empty 
              end
            end
              
          | G.Branch (br, ppt) -> begin fun _ ->
              cil_insert_br ppt br
                cil_tree_empty
            end
              
      end (* walk_last *) ;    
    }


  let of_program ( initflags : Low.initflag list ) (program:P.t) 
      : Cil.global list * Cil.stmt list = 
    
    let pass = 
      fun a -> a, 
        fun b -> b
    in    
    
    (* First things last, Last things first: *)
    (begin fun (gs, ct) ->
       gs,
       begin
         let init_fn    = Cil.Lval (Cil.Var (Low.var_of_op G.Ppt.none Low.Init), Cil.NoOffset) in
         let init_flags = Cil.integer (Low.bits_of_initflags initflags) in
         let init_instr = Cil.Call(None, init_fn, [init_flags], Cil.locUnknown) in
         let ct         = cil_insert (fun _ -> assert false) (`Instr init_instr) ct in
         (cil_block_of_cil_tree ct).Cil.bstmts
       end 
     end 
       : Cil.global list * cil_tree 
      -> Cil.global list * Cil.stmt list)
           
    (* Walk over the program. *)
    **> P.Walk.walk {      
      P.Walk.walk_glob = begin fun glob -> 
        match glob with
          | P.Mark  _ -> pass
          | P.Func  _ -> pass
              
          | P.Var (var, simp, ppt) -> begin fun a -> a, fun (gs, ct) ->
              (* Walk the initializer (a simp) and stash the result
                 into our current cil_tree; The global itself can then be
                 expressed without an initializer---we will move all the
                 initializer code into a designated place. *)
              Cil.GVarDecl(var, G.Ppt.loc ppt) :: gs,
              (walk_simp (fun _ -> assert false) 
                 ppt (lval_of_var var) simp ct)
            end

          | P.Vard (var, ppt) -> fun a -> a,
              begin fun (gs, ct) -> 
                let rest = (Cil.GVarDecl(var, G.Ppt.loc ppt)) :: gs in
                if Annot.is_arrow_type var.Cil.vtype 
                  (* Is the return-type of the function a composite structure?
                     If so, include a global decl for it. *)
                then
                  let (global, _, _) = Annot.arrow_type_unpack var.Cil.vtype in
                  match global with 
                    | Some g -> g :: rest, ct
                    | None   ->      rest, ct
                else                 rest, ct
              end
              
          | P.Cilg (cilg, ppt) -> 
              fun a -> a, 
                (fun (gs, ct) -> (cilg :: gs), ct)

      end (* walk_glob *);
                
      
      P.Walk.walk_func = begin fun (var, vars, graph, ppt) a ->
        let return_type = match var.Cil.vtype with
          | Cil.TFun (t, _, _, _) -> t
          | _                     -> assert false
        in
        
        let stmt_of_label : label -> Cil.stmt =
          let map = 
            List.fold_left begin fun map label ->
              if label = G.entry_label then map else 
                let label, labels, _, _ = G.info_of_label label graph in
                let stmt = Cil.mkEmptyStmt () in
                stmt.labels <- label :: labels ;
                Label.Map.add label stmt map 
            end Label.Map.empty (G.Blocks.labels graph)
          in
          (fun label -> Label.Map.find label map)
        in

        let ct = 
          try
            cil_tree_no_instrs begin G.Walk.walk 
                (graph_walk_fns return_type stmt_of_label graph) 
                [] (G.entry graph) end
          with
            | Last_is_exit ->
                raise (Last_is_exit_in_func var.Cil.vname)
        in
        
        (* SNARKY RANT: The fact that function types contain the
           _names_ of the function's parameters is totally moronic.

           Perhaps there's some good reason for this?  Perhaps to add
           some extra invariants that are easy to forget and
           cumbersome to maintain?  More likely, it's because it makes
           something else simplier (e.g., the representation of
           function prototypes as just a variable, including its
           type), but here's where I make sure that the formal
           variables (given below) match those in the type of the
           function variable.
           
           The formals given in the function type can easily get out
           of sync with those in our function representation when we
           do alpha-conversions of function bodies; I choose to
           confine ugly-invariant enforcement (via an in-place update)
           here: *)
        let _ = 
          var.vtype <- begin match var.vtype with
            | Cil.TFun (_, _, true, _) -> assert false (* no var args *)
            | Cil.TFun (t, Some _, false, atts) ->
                let formals = List.map begin fun var -> 
                  (var.vname, var.vtype, []) end vars
                in
                Cil.TFun (t, Some formals, false, atts)
            | _ -> assert false
          end
        in

        let fundec = {
          Cil.svar     = var  ;
          Cil.sformals = vars ;
          Cil.slocals  = ct.vars [] ;
          
          (* The function body is made from a walk of the graph *)
          Cil.sbody    = cil_block_of_cil_tree ct ;
                                  
          Cil.smaxid = List.fold_left (max) 0
            (List.map (fun v -> v.Cil.vid) (ct.vars vars)) ;

          (* These fields store satellite data computed by
             Cil.computeCFG; we don't have to fill them in
             meaningfully here. *)
          Cil.smaxstmtid = None ;
          Cil.sallstmts  = [] ;
        } in 
        begin a, fun (gs, ct) -> 
          (Cil.GFun (fundec, G.Ppt.loc ppt)) :: gs, ct
        end
      end (* walk_func *);

      P.Walk.walk_root = begin fun graph a -> [], cil_tree_empty
        (* TODO -- what about the root graph? *)
      end (* walk_root *);
    
    } () program
    
end      


module Recursion_as_looping = struct
  (* Only handles singley tail-recursive case---it doesn't change
     mutually-recursive functions into looping. *)

  module G  = Zipcfg
  module B  = G.Build  
  module P  = Program

  (* Is there some argument that we cannot let-bind (that is, that we
     cannot copy from one variable to another).  So far the only case
     in the affirmative are variables with type va_list (see stdarg(3) 
     for evidence of this fact). *)
  let some_argument_cannot_be_let_bound (vars : G.var list) =
    List.exists 
      (function 
         | TSBase (TBuiltin_va_list _) -> true
         | _                           -> false) 
      (List.map (fun v -> Cil.typeSig v.Cil.vtype) vars)      

  let of_program (program:P.t) : P.comb =
    P.Walk.walk
      { P.Copy.walk_fns with 
          P.Walk.walk_func = begin (* Lower function bodies. *)
            fun (fun_var, fun_vars, fun_graph, fun_ppt) above ->   

              let fun_vars', fun_graph'  = 
                if some_argument_cannot_be_let_bound fun_vars then 
                  (* Do nothing. *)
                  (fun_vars, G.entry fun_graph)
                
                else
                
                  (* Get a fresh version for each formal.  We'll use the
                     original formals for the loop body. *)
                  let fun_vars' = List.map Temps.fresh_ver fun_vars in
                  
                  (* let-bind a label to act as a target for the
                     redirected tail calls *)
                  let fun_lab  = Temps.fresh_label (G.Ppt.loc fun_ppt) in
                  
                  (* New argument variables: *)                  
                  fun_vars', 
                
                begin                
                  (* New function body: Place the entire graph under a
                     new block labeled by some label L.  The formal
                     arguments to L correspond to the formal arguments
                     to the function.  Each _tail_call_ to the function
                     becomes a branch to L. *)
                  B.let_block' fun_ppt fun_lab fun_vars begin G.unfocus **>
                      G.Walk.walk
                      { G.Copy.walk_fns with
                          
                          G.Walk.walk_middle =
                          (* A self-targeting call is in tail position
                             iff it is not under any cuts. *)
                          begin fun middle in_tail_position -> match middle with
                            | G.Cut _ -> false,            B.cons_middle middle
                            | _       -> in_tail_position, B.cons_middle middle
                          end ;
                          
                          G.Walk.walk_last = 
                          begin fun last in_tail_position -> match last with
                              
                            (* tail calls to the function change into
                               branches to the label *)
                            | G.Call (G.Default, Cil.Lval (Cil.Var v, Cil.NoOffset), pures, ppt)
                                when in_tail_position && v == fun_var -> 
                                B.branch ppt (G.Br (fun_lab, pures)) G.emptyz
                                  
                            | _ -> B.cons_last last G.emptyz
                                
                          end ;
                          
                      } true (G.entry fun_graph)                
                  end 
                    
                  (* branch to fun_label *)
                  **> B.branch fun_ppt
                    (G.Br (fun_lab, List.map 
                             (fun v -> Cil.Lval (Cil.Var v, Cil.NoOffset)) 
                             fun_vars'))
                  **> G.emptyz
                end
              in
              
              above, begin fun comb z ->
                P.Build.func'' fun_ppt begin
                  fun_var, fun_vars', 
                  (G.unfocus fun_graph'),
                  G.Ppt.from fun_ppt
                end **> comb **> z
              end
          end ; 
      } () program 
end

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(** {3 Compilation driver } *)

module Compile_ceal = struct  
  
  let ( ++ ) = Pretty.( ++ )
    
  module Flags = struct
    let do_cealc     : bool ref = ref true  
    let hobble_along : bool ref = ref false    
    let tv_signals   : bool ref = ref false
      
    (* Qualifiers *)
    let ignore_awar  : bool ref = ref false
    let ignore_zwzr  : bool ref = ref false
    let ignore_owcr  : bool ref = ref false
    let ignore_ring  : bool ref = ref false    
  end
    
  let next_pass_num : unit -> int =
    let pass_num = ref 0 in
    begin fun _ ->
      let n = !pass_num in
      incr pass_num ; n
    end

  let flush out =    
    Printf.fprintf out "%!"

  let full_pass_name (pass_name:string) =
    Printf.sprintf "pass-%02d-%s" (next_pass_num ()) pass_name

  let print_starting (pass_name:string) : unit =
    if ! Global_flags.debug_passes then begin
      Pretty.fprint stderr ~width:80 
        (Pretty.dprintf "%-30s : starting ...@!" pass_name) ;
      flush stderr
    end

  let print_complete (pass_name:string) : unit =
    if ! Global_flags.debug_passes then begin
      Pretty.fprint stderr ~width:80 
        (Pretty.dprintf "%-30s : done.@!" pass_name) ;
      flush stderr
    end
    
  let print_dumped (pass_name:string) (file:string) : unit =
    Pretty.fprint stderr ~width:80 
      (Pretty.dprintf "%-30s : dumped: %s.@!" pass_name file) ;
    flush stderr

  let temp_file_of_prog (pass_name:string) (prog:Program.t) (force_write:bool) =
    if ! Global_flags.debug_passes || force_write then
      let file = Filename.concat (!Global_flags.output_path) (pass_name ^ ".out") in
      let out  = open_out file in
      let doc  = Program.Print.pr_prog prog in
      let _    = Pretty.fprint out ~width:100 doc in
      let _    = close_out out in 
      let _    = print_dumped pass_name file in
      () 

  let temp_file_of_file (pass_name:string) (file:Cil.file) =
    if ! Global_flags.debug_passes then
      let filen =  (Filename.concat (!Global_flags.output_path) (pass_name ^ ".out")) in
      let out   = open_out filen in
      let _     = Cil.dumpFile my_printer out filen file in
      let _     = close_out out in 
      let _     = print_dumped pass_name filen in
      ()

  (* In the spirit of Cil, we side-effect files as we process them *)
  let file_pass (name:string) (passf:Cil.file -> 'a) (file:Cil.file) : 'a =
    let n = full_pass_name name in
    let _ = print_starting n in
    let x = passf file in (* Note: this is a side-effecting transformation! *)
    let _ = temp_file_of_file n file in
    let _ = print_complete n in
    ( x )

  (* Passes that have program combinators as outputs; no side-effects here. *)
  let prog_pass (name:string) (passf:'a -> Program.comb) (a:'a) : Program.t =
    let n    = full_pass_name name in
    let _    = print_starting n in
    let prog = Program.prog_of_frag (passf a Program.empty_frag) in
    let tc   = Type_check.of_program prog in
    let _    = temp_file_of_prog n  prog (!Global_flags.debug_passes || (not tc)) in
    let _    = print_complete n in
    assert (tc || !Flags.hobble_along) ;
    prog
      
  (* Do the phases of compilation that are required exclusively for a
     verifier target. *)
  let compile_verifier prog =
    let prog = prog_pass "Lower_verifier"        Lower_verifier.of_program       prog in
    let prog = prog_pass "Recursion_as_looping"  Recursion_as_looping.of_program prog in
    let prog = prog_pass "Simplify"              Simplify.of_program             prog in
    prog

  (* Do the phases of compilation that are required exclusively for a
     self-adjusting target. *)
  let compile_self_adjusting prog =
    
    (* noop = Identity transformation. *)
    let noop (comb:Program.comb) = comb in

    (* Separate the levels. *)      
    let callgraph  =  Program_levels.Callgraph.of_program prog in
    let meta, core = (Program_levels.Separate.of_program callgraph) prog in
    
    (* Compile the meta-level code. *)
    let meta =
      let meta = prog_pass "Program_levels.Meta" noop                  meta in
      let meta = prog_pass "Lower_meta"          Lower_meta.of_program meta in
      let meta = prog_pass "Simplify"            Simplify.of_program   meta in
      let meta = 
        if ! Flags.tv_signals 
        then prog_pass "Tv_signals" (Tv_signals.of_program `Meta) meta 
        else meta
      in
      meta
    in
    
    (* Compile the core-level code (fixing up the meta program as needed). *)
    let core = 
      let core = prog_pass "Program_levels.Core" noop                core in
      let core = prog_pass "Simplify"            Simplify.of_program core in

      (* Prepare the conventional calls (to foreign C code) for region analysis. *)
      let cut_conv_calls = Program_levels.Cut_conv_calls.of_program callgraph in
      let core = prog_pass "Cut_conv_calls" cut_conv_calls core in
      
      (* Do the region analysis *)
      let rega = Region_analysis.of_program core in
      (* let _    = Region_analysis.print stderr    rega in *)
      
      (* Do destination passing style transformation; then simplify to restore dominator invariant: *)
      let core = prog_pass "Dest_passing"       (Dest_passing.of_program rega)  core in
      let core = prog_pass "Simplify"            Simplify.of_program            core in
      
      (* Do the region analysis (again). *)
      let rega = Region_analysis.of_program core in
      (* let _    = Region_analysis.print stderr    rega in *)
      
      let core = prog_pass "Lower_core"         (Lower_core.of_program rega)    core in
      (* let core = prog_pass "Simplify"            Simplify.of_program            core in *)
      
      let core = 
        if ! Flags.tv_signals 
        then prog_pass "Tv_signals" (Tv_signals.of_program `Core) core 
        else core
      in
      core
    in      
    
    (* Merge the levels: *)
    let prog = Program_levels.Merge.of_programs meta core in
    let prog = prog_pass "Program_levels.Merge" noop prog in

    (* Change all remaining self-targeting tail calls into loops *)
    let prog = prog_pass "Recursion_as_looping"  Recursion_as_looping.of_program prog in
    (* let prog = prog_pass "Simplify"              Simplify.of_program             prog in *)
    
    (* Alpha-vary the local variables -- ensure no duplicate uses of the same name. *)
    let prog = prog_pass "Alpha_vary_locals"     Alpha_vary_locals.of_program    prog in    
    prog

  (* Process a file (assume file contains a complete "core program"). *)
  let process_file (file:file) =        

    Global_flags.output_path 
      := Filename.dirname file.Cil.fileName ;

    if !Flags.do_cealc then
      let _ = if ! Global_flags.announce_that_we_exist 
      then begin
        Printf.fprintf stderr "%s is starting to compile %s into a %s program\n%!"
          (Global_flags.our_name ())
          file.Cil.fileName 
          (Global_flags.string_of_compilation_target ()) ;
      end
      in
      
      let _ = Low.init file in
      
      let _ = file_pass "Source" (fun _ -> ()) file in

      (* Ignore the storage hints that we are told to *)
      let _ = file_pass "Qual.Strip_ignored" 
        (Qual.Strip.process_file (!Qual.Strip.ignored)) file in
      
      let foreign = file_pass "Separate_foreign_c" Separate_foreign_c.process_file  file in
      let _       = file_pass "Heapify_globals"    Heapify_globals.process_file  file in
      let _       = file_pass "Heapify_locals"     Heapify_locals.process_file   file in
      let _       = file_pass "Scalarize_memops"   Scalarize_memops.process_file file in
      let _       = file_pass "Separate_memops"    Separate_memops.process_file  file in
      (*let _     = file_pass "Prepare_cuts"       Prepare_cuts.process_file     file in*)

      (* No more side-effects (except on the heap)... *)
      let prog = prog_pass "Program.From_cil"   Program.From_cil.file         file in
      let prog = prog_pass "Simplify"           Simplify.of_program           prog 
      in
      
      (* Compile the program somehow (either as a SAC program, or a
         verifier program ('conventional' C)) ). *)
      let prog = match ! Global_flags.compilation_target with
        | `Self_adjusting -> compile_self_adjusting prog
        | `Verifier       -> compile_verifier prog
      in

      let _ = 
        (* Flags for Low.Init, the CEAL runtime initialization routine. *)
        let flags = 
          [ ( match ! Global_flags.compilation_target with
                | `Self_adjusting -> Low.Initflag_selfadj
                | `Verifier       -> Low.Initflag_verifier )
          ;
            ( if ! Flags.tv_signals 
              then Low.Initflag_tvsig 
              else Low.Initflag_none ) ]
        in

        (* Convert program back into CIL's representation. *)
        let globs, stmts = To_cil.of_program flags prog in
        begin
          file.Cil.globals  <- globs @ foreign ;
          let init_fundec = Cil.getGlobInit file in
          init_fundec.Cil.sbody.Cil.bstmts <- stmts
        end ;
        
        (* We don't need CEAL attributes anymore-- they will only anger gcc. *)
        file_pass "Qual.Strip_all" ( Qual.Strip.process_file Qual.all ) file ;
        file_pass "Ffi.Strip" Ffi.Strip.process_file file ;

        file_pass "Target" (fun _ -> ()) file ;
        
        if ! Global_flags.announce_that_we_exist then 
          Printf.fprintf stderr "%s is finished.\n" 
            (Global_flags.our_name ())
      in
      ()
        
    else
      (* TEMP: *)
      let _ = file_pass "Annot.Strip" Annot.Strip.process_file file in
      ()

  let feature : Cil.featureDescr = 
    { fd_name = "cealc";
      fd_enabled = Flags.do_cealc ;
      fd_description = "processing of CEAL code (including generation of target C code)";
      (*fd_description = "generation of target C code for embedded CEAL source code";*)
      fd_extraopt = [
        ("--ceal-tv-signals", Arg.Set Flags.tv_signals, 
         "  generate \"TV signals\"; useful for debugging, visualization, etc.");

        ("--ceal-verifier", Arg.Unit (fun _ -> Global_flags.compilation_target := `Verifier),
         " extract 'conventional' (non-SAC) C target code from CEAL source input");

        ("--ceal-hobble", Arg.Set Flags.hobble_along,
         " do not stop compiling when CEAL-IL type errors are encountered");

        ("--ceal-pp", Arg.Set Zipcfg.Print.print_program_points,
         " include program points when printing CEAL-IL");

        ("--ceal-no-pp", Arg.Clear Zipcfg.Print.print_program_points,
         " do not include program points when printing CEAL-IL");

        ("--ceal-singleton-trnodes", Arg.Set Global_flags.singleton_trnodes,
         " each trace node contains exactly one trace action");

        ("--ceal-ignore-qual", Qual.Strip.ignore_arg_spec,
         " ignore the given qualifer; pretend it never appears");
        
        ("--ceal-dest-qual", Qual.Dest.arg_spec,
         " use the specified qualifer for destinations that we introduce");

        ("--ceal-implicit-qual", Qual.Implicit.arg_spec,
         "  use this qualifier whenever one is missing (or being ignored).");
        
        ("--ceal-dps-everything", Arg.Clear Global_flags.dps_is_selective,
         " DPS-convert every region, even those without any reads");
        
        ("--ceal-db-live", Arg.Set Global_flags.debug_live_vars,
         " print live variable info for CEAL-IL");
        
        ("--ceal-db-entrances", Arg.Set Global_flags.debug_entrances,
         " print labels that become 'entrances' during lowering.");

        ("--ceal-db-passes", Arg.Set Global_flags.debug_passes,
         " print passes as they are performed, including files for CEAL-IL");
        
        ("--ceal-db-cil-dom-trees", Arg.Set Global_flags.debug_cil_dom_trees,
         " generate dominator trees for CIL code, before we convert to CEAL-IL");

        ("--ceal-db-dom-trees", Arg.Set Global_flags.debug_dom_trees,
         " generate dominator trees for CEAL-IL code");
      ];
      fd_doit = process_file;
      fd_post_check = true;      
    }
end

let feature = Compile_ceal.feature
  
