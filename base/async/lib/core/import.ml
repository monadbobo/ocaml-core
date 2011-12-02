open Core.Std

module Error = struct
  include Error

  let create message a sexp_of_a = arg message sexp_of_a a
end

let sexp_of_phantom _ = assert false

let fail message a sexp_of_a = Error.raise (Error.create message a sexp_of_a)
