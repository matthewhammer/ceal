module Tvs = Tv_signal
open Printf

let rec string_of_list delim f = function
  | []    -> ""
  | [v]   -> f v
  | v::vs -> delim (f v) (string_of_list delim f vs)

let string_of_env : Tvs.c_env -> string = fun env ->
  "{" ^
    string_of_list (fun a b -> sprintf "%s, %s" a b)
    (fun (x,v,t) -> sprintf "%s : %s |--> %s" x t v) env
  ^ "}"
    
let string_of_vals : Tvs.c_vals -> string = fun vals ->
  "<" ^
    string_of_list (fun a b -> sprintf "%s, %s" a b) 
    (fun (v,t) -> sprintf "%s : %s" v t) vals
  ^ ">"

let string_of_action_id (aid:Tvs.action_id) =
  ( sprintf "<%s,%s>" (fst aid) (snd aid) )

module Vt100 = struct
            
  (* Set display attributes, then reset to normal. *)
  let vt100_da atts s = sprintf "\x1B[%sm%s\x1B[0m" atts s  
    
  (* Escape prefixes *)
  let vt100_es   s = sprintf "\x1B%s" s
    
  let normal       = vt100_da "0"
  
  let bright       = vt100_da "1"    
  let dim          = vt100_da "2"
  let blink        = vt100_da "5"
  let reverse      = vt100_da "7"
  let underscope   = vt100_da "4"

  let bright_green = vt100_da "1;32"
    
  let white        = vt100_da "37"
  let cyan         = vt100_da "36"
  let magenta      = vt100_da "35"
  let blue         = vt100_da "34"
  let yellow       = vt100_da "33"
  let green        = vt100_da "32"
  let red          = vt100_da "31"
  let black        = vt100_da "30"
    
  let black_b      = vt100_da "40"
  let red_b        = vt100_da "41"
  let green_b      = vt100_da "42"
  let yellow_b     = vt100_da "43"
  let blue_b       = vt100_da "44"
  let magenta_b    = vt100_da "45"
  let cyan_b       = vt100_da "46"
  let white_b      = vt100_da "47"
  
  let newpage      = "\x0c"

  let alt          = vt100_da ")"
  let home         = vt100_es "[H"
  let erase_screen = vt100_es "[2J"
  let erase_west   = vt100_es "[K"
  let erase_down   = vt100_es "[J"

end

include Vt100


let three_chars_of_action (ppt, aid, sz, desc) = match desc with
  | `A_alloc (t,sz,ptr)  -> sprintf " A "
  | `A_scope (ptr)       -> sprintf " S "
  | `A_read (t,q,ptr,v)  -> sprintf " R "
  | `A_write (t,q,ptr,v) -> sprintf " W "
  | `A_memo (env)        -> sprintf " M "
  | `A_update (env)      -> sprintf " U "
  | `A_tcall (f,vals)    -> sprintf " tc"
  | `A_pop (vals)        -> sprintf "pop"
  | `A_push_begin        -> sprintf " Pb"
  | `A_push_end          -> sprintf " Pe"
  | `A_end               -> sprintf " E "
      
let chars_of_action (ppt, aid, sz, desc) = match desc with
  | `A_alloc (t,sz,ptr)  -> sprintf "A"
  | `A_scope (ptr)       -> sprintf "S"
  | `A_read (t,q,ptr,v)  -> sprintf "R"
  | `A_write (t,q,ptr,v) -> sprintf "W"
  | `A_memo (env)        -> sprintf "M"
  | `A_update (env)      -> sprintf "U"
  | `A_tcall (f,vals)    -> sprintf "tc"
  | `A_pop (vals)        -> sprintf "p"
  | `A_push_begin        -> sprintf "Pb" 
  | `A_push_end          -> sprintf "Pe"
  | `A_end               -> sprintf "E"

let short_of_action (ppt, aid, sz, desc) = match desc with
  | `A_alloc (t,sz,ptr)  -> sprintf "alloc"
  | `A_scope (ptr)       -> sprintf "scope"
  | `A_read (t,q,ptr,v)  -> sprintf "read"
  | `A_write (t,q,ptr,v) -> sprintf "write"
  | `A_memo (env)        -> sprintf "memo"
  | `A_update (env)      -> sprintf "update"
  | `A_tcall (f,vals)    -> sprintf "tcall"
  | `A_pop (vals)        -> sprintf "pop"
  | `A_push_begin        -> sprintf "pushb" 
  | `A_push_end          -> sprintf "pushe"
  | `A_end               -> sprintf "end"

let string_of_action_desc = function
  | `A_alloc (t,sz,ptr)  -> sprintf "alloc  (%s, %d, %s)" t sz ptr
  | `A_scope (ptr)       -> sprintf "scope  (%s)" ptr
  | `A_read (t,q,ptr,v)  -> sprintf "read   (%s, %s, %s, %s)" t q ptr v
  | `A_write (t,q,ptr,v) -> sprintf "write  (%s, %s, %s, %s)" t q ptr v
  | `A_memo (env)        -> sprintf "memo   (%s)" (string_of_env env)
  | `A_update (env)      -> sprintf "update (%s)" (string_of_env env)
  | `A_tcall (f,vals)    -> sprintf "tcall  (%s, %s)" f (string_of_vals vals)
  | `A_pop (vals)        -> sprintf "pop    (%s)" (string_of_vals vals)
  | `A_push_begin        -> sprintf "push_begin"
  | `A_push_end          -> sprintf "push_end"
  | `A_end               -> sprintf "end"

let string_of_action (ppt, aid, sz, desc) = 
  let desc_string = string_of_action_desc desc in
  sprintf "%-60s %-25s (%2d) %s" 
    ( sprintf "%-30s: %4d: %s:"
        (Filename.basename ppt.Tvs.ppt_file) 
        ppt.Tvs.ppt_line ppt.Tvs.ppt_fname )
    ( string_of_action_id aid )
    ( sz )
    ( desc_string )

let string_of_step = function
  | `S_invoke action -> string_of_action action
  | `S_revinv action -> string_of_action action
  | `S_revoke aid    -> sprintf "revoke %s" (string_of_action_id aid)
  | step             -> Tv_signal.string_of_step step

let string_of_event = function
  | `E_step step -> string_of_step step
  | event        -> Tv_signal.string_of_event event


(* Print monad *)
(* TODO -- make this into a real monad. *)
module Print = struct
  
  (* Printing parameters *)
  type t = { 
    stack_depth : int ;
    print_ppt   : (Tvs.ppt -> string) option ;
  }
      
  (* Default printing parameters *)
  let defaults = {
    stack_depth = 0;
    print_ppt   = Some (fun ppt -> sprintf "%-30s: %4d: %s:"
                          (Filename.basename ppt.Tvs.ppt_file) 
                          ppt.Tvs.ppt_line ppt.Tvs.ppt_fname)
  }
    
  let print_action_desc (p:t) = function
    | `A_alloc (t,sz,ptr)  -> p, p, sprintf "alloc  (%s, %d, %s)" t sz ptr
    | `A_scope (ptr)       -> p, p, sprintf "scope  (%s)" ptr
    | `A_read (t,q,ptr,v)  -> p, p, sprintf "read   (%s, %s, %s, %s)" t q ptr v
    | `A_write (t,q,ptr,v) -> p, p, sprintf "write  (%s, %s, %s, %s)" t q ptr v
    | `A_memo (env)        -> p, p, sprintf "memo   (%s)" (string_of_env env)
    | `A_update (env)      -> p, p, sprintf "update (%s)" (string_of_env env)
    | `A_tcall (f,vals)    -> p, p, sprintf "tcall  (%s, %s)" f (string_of_vals vals)
    | `A_pop (vals)        -> p, p, sprintf "pop    (%s)" (string_of_vals vals)
    | `A_push_begin        -> ( p, { p with stack_depth = p.stack_depth + 1}, 
                                sprintf "push_begin" )
    | `A_push_end          -> ( { p with stack_depth = p.stack_depth - 1}, 
                                { p with stack_depth = p.stack_depth - 1},
                                sprintf "push_end" )
    | `A_end               -> p, p, sprintf "end"
    (*| ad                   -> p, p, (Tv_signal.string_of_action_desc ad)*)
        

  let print_action (p:t) (ppt, aid, sz, desc) = 
    let p1, p2, desc_string = print_action_desc p desc in
    let s = sprintf "%-60s %-25s (%2d) %s%s" 
      ( match p1.print_ppt with Some f -> f ppt | None -> "")
      ( string_of_action_id aid )
      ( sz )
      ( let rec loop space indent = 
          if indent > 0 then loop (space^"| ") (indent - 1) 
          else space 
        in loop ": " p1.stack_depth )
      ( desc_string )
    in
    p2, s

  let lift f (p,s) = (p, f s)

  let string_of_step (p:t) = function
    | `S_invoke action -> lift green (print_action p action)
    | `S_revinv action -> lift bright_green (print_action p action)    
    | `S_revoke aid    -> lift red (p, sprintf "revoke %s" (string_of_action_id aid))
    | step             -> p, (Tv_signal.string_of_step step)
        

  let string_of_event (p:t) = function
    | `E_step step -> string_of_step p step
    | event        -> p, Tv_signal.string_of_event event

  let print_event (p:t) = function
    | `E_step step -> let p, s = string_of_step p step in ( printf "%s" s ; p )
    | event        -> printf "%s" (Tv_signal.string_of_event event); p

end
