open Std_internal

INCLUDE "config.mlh"
IFDEF ARCH_x86_64 THEN
external backtrace_get : unit -> string = "backtrace_get"
let get = Ok backtrace_get
ELSE
let get = Or_error.error "unimplemented" "Backtrace.get" <:sexp_of< string >>
ENDIF
