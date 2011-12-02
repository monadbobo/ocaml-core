
(** A [Bounded_int_table] is a table where the keys are distinguished by unique integers
    in [0, num_keys).  The purpose of [Bounded_int_table] is to be faster than an [('a,
    'b) Hashtbl.t] in situations where one is willing to pay a space cost for the speed.

    [Bounded_int_table] presents a subset of the [Hashtbl] interface.  Any operation that
    supplies a key outside the range of keys for the table will cause an exception.

    A [Bounded_int_table] is implemented using a fixed size array, so lookup is constant
    time.  The space used does not depend on the [length] of the table but rather only on
    [num_keys].

    Operations that deal with a single element (find, mem, remove, replace) take constant
    time, and perform one or two array operations.  Operations that deal with all of the
    keys defined in the table (data, fold, iter, iter_vals, keys, to_alist) take time
    proportional to the number of defined keys, not [num_keys]. *)

open Std_internal

type ('key, 'data) t

val invariant : (_, _) t -> unit

val sexp_of_t : ('a1 -> Sexp.t) -> ('a2 -> Sexp.t) -> ('a1, 'a2) t -> Sexp.t

(** [create ~num_keys ~key_to_int] returns a table where the allowed keys are
    0 .. num_keys-1.  It is an error if [num_keys < 0]. *)
val create : num_keys:int -> key_to_int:('key -> int) -> ('key, 'data) t

(** Standard hashtbl functions. *)
val data : (_, 'data) t -> 'data list
val find : ('key, 'data) t -> 'key -> 'data option
val fold :
  ('key, 'data) t
  -> init:'accum
  -> f:(key:'key -> data:'data -> 'accum -> 'accum)
  -> 'accum
val iter : ('key, 'data) t -> f:(key:'key -> data:'data -> unit) -> unit
val iter_vals : (_, 'data) t -> f:('data -> unit) -> unit
val keys : ('key, _) t -> 'key list
val length : (_, _) t -> int
val mem : ('key, _) t -> 'key -> bool
val remove : ('key, _) t -> 'key -> unit
val replace : ('key, 'data) t -> key:'key -> data:'data -> unit
val to_alist : ('key, 'data) t -> ('key * 'data) list

(** set [debug := true] to turn on debugging, including potentially slow invariant
    checking. *)
val debug : bool ref
