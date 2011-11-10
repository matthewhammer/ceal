
module A = Hic_ast

module I = Hic_identifier
type id = I.id

exception NYI
 
(* evaluation context *)
type ctxt = { 
  path  : id list  ;
  subst : I.Subst.t  ;
  store : mval I.M.t ;
}

(* module value *)      
and mval = {
  mv_ctxt  : ctxt ;
  mv_desc  : mval_desc ;
  mv_trace : trace ;
}

(* trace of a module value *)
(* TODO -- include program points from the AST. *)
and trace = 
  | Tr_app  of mval * mval
  | Tr_abs  of id * mtype * mexp
  | Tr_bind of id * mval
  | Tr_seal of mval * tc
  | Tr_body of mexp
    
(* module value *)
and mval_desc = 
  | Mv_body of A.mecomp list
  | Mv_abs  of id * A.mtype * A.mexp

(* type check *)
and tc = Ok of A.mtype

(* type-checking could eliminate this exception: *)
exception Not_a_functor of ctxt * mval
exception Not_a_module of ctxt * id

let type_check ctxt mval mtype : tc =
  raise NYI

let ctxt_ext ctxt id mval mtype = 
  let Ok _ = type_check mval mtype in
  raise NYI

let ctxt_proj ctxt id : mval =
  raise NYI
  
let eval_mexp ctxt = function
  | A.Me_seal ( mexp, mtype ) -> 
      let mval = eval_mexp ctxt mexp in
      let tc   = type_check ctxt mval mtype in
      { mval with trace = Tr_seal(mval, ty)

  | A.Me_abs ( id, mtype, mexp ) ->
      { mv_ctxt  = ctxt ;
        mv_desc  = Mv_abs ( id, mtype, mexp ) ;
        mv_trace = Tr_abs ( id, mtype, mexp ) ; }

  | (A.Me_body mecomps) as mexp ->
      { mv_ctxt  = ctxt ;
        mv_desc  = Mv_body mecomps ;
        mv_trace = Tr_body mexp ; }
                 
  | A.Me_id id -> 
      ( match I.Subst.app id ctxt.subst with
          | ( name, I.Ik_mod, prov ) ->
              
              
      )
          
  | A.Me_app (mexp1, mexp2) ->
      let mval1 = eval_mexp mexp1 ctxt in
      let mval2 = eval_mexp mexp2 ctxt in
      ( match mval1.mv_desc with
        | Mv_body _ -> raise (Not_a_functor(ctxt,mval1))
        | Mv_abs (id, mtype, mexp) ->
            let ctxt' = ctxt_ext ctxt id mval2 mtype in
            let mval3 = eval_mexp ctxt mexp in
            { mval3 with trace = Tr_app (mval1, mval2) } )
        
        
end
