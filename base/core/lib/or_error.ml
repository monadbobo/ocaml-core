open Result.Export

type 'a t = ('a, Error.t) Result.t with sexp, bin_io

let try_with f =
  try Ok (f ())
  with exn -> Error (Error.of_exn exn)

let try_with_bind f =
  try f ()
  with exn -> Error (Error.of_exn exn)

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err

let of_exn exn = Error (Error.of_exn exn)

let error string a sexp_of_a = Error (Error.create string a sexp_of_a)
