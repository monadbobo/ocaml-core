type t with sexp_of

val create : (unit -> unit) -> t

val run : t -> unit
