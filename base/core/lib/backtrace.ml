module Sexp = Sexplib.Sexp
module List = Core_list
module String = Core_string

type t = string

let to_string t = t

let sexp_of_t t =
  let l =
    List.map (String.split t ~on:'\n') ~f:(fun s ->
      Sexp.Atom
        (match String.index s ':' with
        | None -> s
        | Some i -> try String.slice s (i + 2) 0 with _ -> s))
  in
  Sexp.List (List.drop l 2)
;;

external get : unit -> string = "backtrace_get"
