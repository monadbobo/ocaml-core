
(** A data-structure to loop through a list of values using round-robin *)

type 'a t

include Sexpable.S1 with type 'a t := 'a t

val create : 'a array -> ('a t, string) Result.t

val elements : 'a t -> 'a array

(** Return the next element, round-robin style *)
val next : 'a t -> 'a

(** Return the next element that satisfies the predicate ~f, or None if all elements
    failed the test. *)
val find_next : 'a t -> f:('a -> bool) -> 'a option
