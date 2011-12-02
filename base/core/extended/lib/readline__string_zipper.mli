(* This is an internal module: it should not be used by anything out of
   core_extended *)
open Core.Std

type t

val contents       : t -> string

val left_contents  : t -> string
val right_contents : t -> string

val insert_before  : t -> char -> t
val drop_before    : t -> (char * t) option
val drop_after     : t -> (char * t) option
val previous       : t -> t option
val next           : t -> t option
val first          : t -> t
val last           : t -> t

val create         : string -> string -> t
