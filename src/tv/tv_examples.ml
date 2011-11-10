open Tv_signal

let ppt = { ppt_id   = 666 ;
	    ppt_line = -1 ;
	    ppt_byte = -1 ;
	    ppt_file = "elif" ;
	  }
  
let env = [ ("hd","0","int") ]

let ex1 = `E_step (`S_invoke (ppt, ("100", "101"), 4, `A_memo env))

let ex2 = `E_step (`S_invoke (ppt, ("100", "105"), 3, `A_alloc ("struct cons_s", 1, "115")))

let ex3 = `E_step (`S_invoke (ppt, ("100", "108"), 1, `A_pop [("","struct cons_s*")]))
    
let examples = [ ex1 ; ex2 ; ex3 ]
  
let _ =   
  let outbuf = Bi_outbuf.create_channel_writer stdout in
    List.iter begin fun ev ->
      write_event outbuf ev ;
      Bi_outbuf.flush_channel_writer outbuf ;
      output_string stdout "\n" ;
      flush stdout ;
    end examples

