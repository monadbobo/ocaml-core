(* Dumping of OCaml stack backtraces at runtime. *)

(* A backtrace is a string with newlines separating the frames.  [sexp_of_t] splits the
   string at newlines and removes some of the cruft, leaving a human friendly list of
   frames, but [to_string] does not. *)
type t with sexp_of

val get : (unit -> string) Or_error.t

val to_string : t -> string
