
open Std_internal

type 'a t =
  { elements : 'a array;
    mutable current_idx : int;   (* index of the last considered element *)
  }

let create elements =
  if elements = [||]
  then Error "Elements array is empty"
  else
    Ok { elements;
         current_idx = Array.length elements - 1;
       }

let elements t = t.elements

(* the S-expression representation is simply the one of the elements array *)
let t_of_sexp a_of_sexp sexp =
  Result.failwith_error (create (array_of_sexp a_of_sexp sexp))
let sexp_of_t sexp_of_a t =
  sexp_of_array sexp_of_a t.elements

(* move pointer to the next element *)
let advance_idx t =
  t.current_idx <- t.current_idx + 1;
  if t.current_idx = Array.length t.elements then t.current_idx <- 0

let next t =
  advance_idx t;
  t.elements.(t.current_idx)

let find_next t ~f =
  let n = Array.length t.elements in
  let rec loop num_trials =
    if num_trials = n
    then None
    else
      let x = next t in
      if f x
      then Some x
      else loop (num_trials + 1)
  in
  loop 0
