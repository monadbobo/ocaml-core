type t = 
  | Realtime
  | Monotonic
  | Process_cpu
  | Process_thread

val all : t list

val to_string : t -> string

(* returns the resulution of the given clock in nanoseconds *)
val getres : t -> int

(* returns the current value of the given clock in nanoseconds *)
val gettime : t -> int

(* sleeps the current thread for the specified number of nanoseconds *)
(*val nanosleep : t -> int -> unit*)

(* [min_interval t] returns the minimum measurable interval for t in nanoseconds *)
val min_interval : t -> int

(* [cost t] returns the cost of calling gettime with the given t int nanoseconds *)
val mean_gettime_cost : measure:t -> using:t -> int
