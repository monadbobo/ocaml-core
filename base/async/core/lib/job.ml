open Core.Std

type t = unit -> unit

let sexp_of_t = sexp_of_a

let run t = t ()

let create work = work
