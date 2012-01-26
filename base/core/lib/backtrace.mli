(* Dumping of OCaml stack backtraces at runtime. *)

val get : (unit -> string) Or_error.t
