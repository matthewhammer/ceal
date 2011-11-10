(* Some simple, commonly-used definitions that I want everywhere. *)

exception NYI

let ( <** ) f g = ( fun x -> f ( g ( x ) ) ) 
let ( **> ) f g = ( fun x -> g ( f ( x ) ) ) 

module type MONAD = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ret  : 'a -> 'a m
end

module Monad_shorthands (M : MONAD) = 
struct
  let ( >>= )     = M.bind
  let ( >>  ) m f = M.bind m (fun _ -> f ())
  let ( >>> ) m n = M.bind m (fun _ -> n)
end

