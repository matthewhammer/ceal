let pdu =
   "pdu-53-11.mpi-sws.org"
;;

let rec compose_command cmd_ind =
   if snd( cmd_ind ) < 61 then compose_command (fst( cmd_ind )
                                                 ^ " .iso.org.dod.internet.private.enterprises."
                                                 ^ "raritan.pdu.board.dataLog.dataLogTable."
                                                 ^ "dataLogEntry.dataLogTimeStamp."
                                                 ^ string_of_int( snd( cmd_ind ) )
                                                 ^ " .iso.org.dod.internet.private.enterprises."
                                                 ^ "raritan.pdu.board.dataLog.dataLogTable."
                                                 ^ "dataLogEntry.dataLogActivePower."
                                                 ^ string_of_int( snd( cmd_ind ) ),
                                                 snd( cmd_ind ) + 1)
   else fst( cmd_ind )
;;

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

let main =
   let command = "snmpget -v 2c -c public " ^ pdu in
   print_string ( syscall (compose_command (command, 1)) )
;;
