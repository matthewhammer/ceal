(* Abstract Syntax *)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
include Hic_identifier

type code = string

(* C qualifiers *)
type cqual =
  | Cqual_id  of id

(* C types *)
type ctype =
  | Ctype_qual   of ctype * cqual
  | Ctype_base   of id
  | Ctype_name   of id
  | Ctype_struct of id
  | Ctype_ptr    of ctype
  | Ctype_fun    of ctype list * ctype

(* - - - - - - - - - - - - - - - - - - - *)
(* module types *)
and mtype =
  | Mt_abs  of id * mtype * mtype
  | Mt_body of mtcomp list
  | Mt_id   of id

(* module-type components *)
and mtcomp =
  | Mtc_type    of id
  | Mtc_qual    of id
  | Mec_mod     of id * (id * mtype) list * mtype
  | Mec_modtyp  of id * mtype option
  | Mtc_open    of id
  | Mtc_include of mtype
  | Mtc_val     of id * ctype

(* - - - - - - - - - - - - - - - - - - - *)
(* module expressions *)
type mexp =
  | Me_seal of mexp * mtype
  | Me_abs  of id * mtype * mexp
  | Me_app  of mexp * mexp
  | Me_body of mecomp list
  | Me_id   of id

(* module-expr components *)
and mecomp =
  | Mec_type    of id * ctype
  | Mec_qual    of id * cqual
  | Mec_mod     of id * mexp  
  | Mec_modtyp  of id * mtype
  | Mec_open    of id
  | Mec_include of mexp
  | Mec_code    of code

