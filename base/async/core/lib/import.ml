open Core.Std

let sexp_of_phantom _ = assert false

let fail message a sexp_of_a = Error.raise (Error.create message a sexp_of_a)
