/<<
module List = {

  module type Arg = {
    type hd_t;
    qual tl_q;
  }
  
  module Make (A : Arg) = {
    include A
    >>/
    typedef struct cons* t;
    typedef t tl_q tl_t;
    struct cons_cell { hd_t hd; tl_t tl; };
    
    t cons(hd_t hd) {
      t c = alloc(*t);
      c->hd = hd;
      return c;
    }
    
    hd_t hd(t l) {
      return l->hd;
    }
    
    tl_t* tl(t l) {
      return &(l->tl);
    }
    <</
  }
  
}
