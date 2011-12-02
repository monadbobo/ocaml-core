(** Simple implementation of a polymorphic functional queue.
    Push and top and bottom are O(1).  Pop and take are O(1) amortized.
    to_list is O(n). length is O(1).
*)

exception Empty

type 'a t with bin_io, sexp

(** test via asserts whether invariants hold *)
val test_invariants : 'a t -> unit

(** The empty queue *)
val empty : 'a t

(** push a single element on queue *)
val push : 'a -> 'a t -> 'a t

(** push a single element on the *top* of the queue *)
val push_top : 'a -> 'a t -> 'a t

(** alias for push *)
val enq : 'a -> 'a t -> 'a t

(** returns the bottom (most-recently enqueued element).  Raises [Empty] if no element is
    found. *)
val bot_exn : 'a t -> 'a

(** like [bot_exn], but returns result optionally, without exception *)
val bot : 'a t -> 'a option

(** Like [bot_exn], except returns top (least-recently enqueued element *)
val top_exn : 'a t -> 'a

(** like [top_exn], but returns result optionally, without exception *)
val top : 'a t -> 'a option

(** Like [top_exn], but returns pair of the top element, and a new queue with the top element
    removed *)
val pop_exn : 'a t -> 'a * 'a t

(** Like [pop_exn], but returns result optionally, without exception *)
val pop : 'a t -> ('a * 'a t) option

(** alias for pop *)
val deq : 'a t -> ('a * 'a t) option

(** alias for pop_exn *)
val deq_exn : 'a t -> 'a * 'a t

(** Returns version of queue with top element removed *)
val discard_exn : 'a t -> 'a t

(** [to_list t] returns a list of the elements in [t] in order from
    least-recently-added (at the head) to most-recently added (at the tail). *)
val to_list : 'a t -> 'a list
val length : 'a t -> int
val is_empty : 'a t -> bool

