let _ =
  let outbuf = Bi_outbuf.create_channel_writer stdout in
    while true do try
      Tv_signal.write_event outbuf (Tv_signal.event_of_string (read_line ())) ;
      Bi_outbuf.flush_channel_writer outbuf ;
      print_string "\n" ;
      flush stdout ;      
    with 
        End_of_file -> exit 0
    done
