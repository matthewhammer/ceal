(* Identifiers *)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Provides provenance information to the programmer about how we
   synthesize their code.  In particular, our synthesis process
   primarily generates and manages names, i.e., identifiers for
   functions, names for types, etc.  Hence, the type of this
   provenance information is parameterized by another type that
   represents these names (types id and id'). *)
  
type id      = string * id_kind * prov
and  id'     = string * id_kind  
and  id_kind = 
  | Ik_mod
  | Ik_modtyp
  | Ik_val
  | Ik_qual
  | Ik_ctype_base
  | Ik_ctype_name
  | Ik_ctype_struct  
      
and prov = 
  | Builtin
  | Source  of source_location
  | Subst   of source_location * id (* for *) * id'
      
and source_location = {
  file : string ;
  line : int ;
  char : int ;
  path : id list ;
}
    
module M = Map.Make (
  struct 
    type t = id' 
    let compare = compare
  end 
)
  
(* Get the pre-id version of an id *)
let pre (name,kind,_) = (name,kind)
  
  
(* Substition -- this encompases the primary operational feature of
   our module semantics: the substitution of names for other names.
   Only those names with "ground provenance" (Source or Builtin)
   have values in the operational environment of this semantics. 
*)
module Subst = struct
  open M
  type t = id M.t
      
  let app = find
    
  exception Undefined_id of id
    
  let is_fixpoint id' subst = (pre (app id' subst)) = id'
    
  (* extend the substitution with a new mapping *)
  let ext loc id id' subst = 
    try 
      if is_fixpoint (pre id) subst then 
        M.add id' id subst
      else 
        let (name,kind,_) = id in
        M.add id' (name, kind, Subst (loc, id , id')) subst
    with
      | Not_found -> raise (Undefined_id id)
          
  let add_builtin ((name,kind) as id) = 
    M.add id (name, kind, Builtin) 
      
  let add_source ((name,kind) as id) loc =
    M.add id (name, kind, Source loc)
      
  let default : t = (
    let ctype_base_names =
      ["void"; "int"; "unsigned"; "long"; "double"; "float"; "signed"]
    in
    List.fold_right 
      (fun name env -> 
         add_builtin (name, Ik_ctype_name) (
           add_builtin (name, Ik_ctype_base) (
             env
           ) ) )
      ctype_base_names empty
  )            
end
