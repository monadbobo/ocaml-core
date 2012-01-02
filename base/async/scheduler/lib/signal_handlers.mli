open Core.Std
open Import

type t with sexp_of

val create : thread_safe_notify_signal_delivered:(unit -> unit) -> t

type z
val install_handler : t -> Signal.t list -> (Signal.t -> unit) -> z
val remove_handler : t -> z -> unit

val handle_signal : t -> Signal.t -> unit

val handle_delivered : t -> unit
