(* This is an internal module it shouldn't be used outside of core_extended *)
open Core.Std

type 'a t = {
  l : 'a list;
  r : 'a list
}

val create : 'a list -> 'a list -> 'a t
val drop_before : 'a t -> ('a * 'a t) option
val drop_after : 'a t -> ('a * 'a t) option
val insert_before : 'a t -> 'a -> 'a t
val insert_after : 'a t -> 'a -> 'a t
val previous : 'a t -> 'a t option
val next : 'a t -> 'a t option
