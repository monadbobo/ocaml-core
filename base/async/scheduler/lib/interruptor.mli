(* An interruptor provides a file descriptor that can be used to cause functions like
   [select] to return due the file descriptor being available for reading. *)

open Core.Std
open Import

type t with sexp_of

val create : Fd_by_descr.t -> t
val read_fd : t -> Raw_fd.t
val thread_safe_interrupt : t -> unit
val clear : t -> unit
