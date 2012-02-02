open Core.Std

let debug = is_some (Sys.getenv "DEBUG_ASYNC")

(* Calls to [Debug.print] should look like [if debug then Debug.print ...]. *)

let log message a sexp_of_a =
  eprintf "%s\n%!"
    (Sexp.to_string_mach
       (<:sexp_of< Pid.t * int * Time.t * string * a >>
           (Unix.getpid (), Thread.id (Thread.self ()), Time.now (), message, a)))
;;

let log_string message = log message () <:sexp_of< unit >>
