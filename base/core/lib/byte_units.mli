(* Conversions between units of measure based on bytes. *)

module Measure : sig
  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
end

type t

(* [create measure float] creates a [t] that will use [measure] when converting [t] to
   a string or sexp. *)
val create : Measure.t -> float -> t

include Binable.S with type t := t
include Comparable.S with type t := t
include Hashable.S with type t := t
include Sexpable.S with type t := t
include Stringable.S with type t := t

val bytes     : t -> float
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val words     : t -> float
