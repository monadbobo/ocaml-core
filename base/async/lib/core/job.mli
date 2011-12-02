type t with sexp_of

val create : (unit -> unit) -> t

val id : t -> int

val run : t -> unit
