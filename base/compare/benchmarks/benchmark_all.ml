open Core.Std

(* this benchmarks three different operations versus their corresponding automatically
   generated ones: compare, (<), (=). There are three tests:

   - array of different_values : this produces an array of different values. The size of
   the array, called len, depends upon the type being checked. Every ith element is
   checked against the (len - i - 1)th element

   - same values : this takes the same randomly generated value, copies it, then compares
   it over and over again

   -two different values: this takes two random values and compares them.

   All the values are computed before the benchmark starts.
*)


module type Benchable =
sig
  type t
  val compare : t -> t -> int
  val description : string
  val create_single : unit -> t
  val copy : t -> t
  val sufficient_sample : int
end

module Bench_int : Benchable =
struct
  type t = int with compare
  let description = "int"
  let create_single () = Random.int 100
  let copy x = x
  let sufficient_sample = 1000
end





module Bench_nested : Benchable =
struct
  type t = ((((int * int) * int) * int) * int) * int with compare,random
  let description = "int nested pairs"
  let copy (((((a,b),c),d),e),f) = (((((a,b),c),d),e),f)
  let create_single = random_of_t
  let sufficient_sample = 1000
end



module Bench_float : Benchable =
struct
  type t = float with compare
  let description = "float"
  let create_single () = Random.float 1000.
  let copy x = x
  let sufficient_sample = 1000
end

module Bench_nan : Benchable =
struct
  type t = float with compare
  let description = "NaN"
  let create_single () =  nan
  let copy _ = nan
  let sufficient_sample = 1000
end

module Bench_variant : Benchable =
struct
  type t =  A | B | C | D | E | F | G | H | I | J | K | LMNOP | Q | R | S | T | U | V |
      WXYZ
  with compare,random
  let description = "simple enumeration"
  let create_single () =  random_of_t ()
  let copy x = x
  let sufficient_sample = 1000
end

module Bench_float_array : Benchable =
struct

  type t = float array with compare
  let description = "float array"
  let create_single () =
    Array.init (Random.int 100) ~f:fun _ -> Random.float 1000.
  let copy arr = Array.init (Array.length arr) ~f:(fun i -> Array.get arr i)
  let sufficient_sample = 1000
end

module Bench_float_ref : Benchable =
struct
  type t = float ref with compare
  let description = "float ref"
  let create_single () =
    ref (Random.float 1000.)
  let copy x = ref (!x)
  let sufficient_sample = 1000
end


module Bench_lambda : Benchable =
struct
  type t =
    | App of t * t
    | Var of string
    | Lambda of string * t with compare,random

  let rec create_random_lambda size =
    let flip = Random.int size in
    if flip = 0 then Var "foo"
    else if flip = 1 then Lambda ("bar",create_random_lambda (size - 1))
    else App (create_random_lambda (size - 1), create_random_lambda (size - 1))

  let description = "lambda calculus"

  let rec copy =
    function
      | App(x,y) -> App(copy x, copy y)
      | Var x -> Var (String.copy x)
      | Lambda (x,t) ->
        Lambda (String.copy x,copy t)

  let sufficient_sample = 5

  let create_single () = create_random_lambda 15

end

module Bench_record : Benchable =
struct
  type t =
      {a : int ; b : int ; c : int ; d : int ; e : int} with random, compare

  let create_single () = random_of_t ()
  let description = "record"
  let copy {a=a;b=b;c=c;d=d;e=e} = {a=a;b=b;c=c;d=d;e=e}
  let sufficient_sample = 1000
end


module Bench_mix : Benchable =
struct
  type t =
      {a : int option ; b : t option ; c : t list option ; d : int ; e : int} with
        random, compare


  let create_single () = random_of_t ()
  let description = "mix of stuff"
  let copy {a=a;b=b;c=c;d=d;e=e} = {a=a;b=b;c=c;d=d;e=e}
  let sufficient_sample = 1000
end





module Bench (T : Benchable) =
struct
  let create_many ~f n =
    let rec loop i sofar =
      if i = n then sofar else loop (i+1) (f () :: sofar)
    in
    Array.of_list (loop 0 [])

  let time f =
    let start = Unix.gettimeofday () in
    f ();
    let finish = Unix.gettimeofday () in
    finish -. start

  let array_test  =
    let arr = create_many ~f:T.create_single T.sufficient_sample in
    let run cmp cmp' =
      let length = Array.length arr in
      let do_it cmp () =
        for j = 1 to 1000 do
          for i = 1 to length - 1 do
            let x = Array.get arr i in
            let y = Array.get arr (length - i - 1) in
            ignore (cmp x y)

          done
        done
      in
      (time (do_it cmp), time (do_it cmp'))
    in
    (10,run)


  let same_test =
    let x = T.create_single () in
    let y = T.copy x in
    let run cmp cmp' =
      let do_it cmp () =
        for i = 0 to T.sufficient_sample * 1000 do
          ignore (cmp x y)
        done
      in
      (time (do_it cmp), time (do_it cmp'))
    in
    (10, run)

  let different_test =
    let x = T.create_single () in
    let y = T.create_single () in
    let run cmp cmp'  =
      let do_it cmp () =
        for i = 0 to T.sufficient_sample * 1000 do
          ignore (cmp x y)
        done
      in
      (time (do_it cmp), time (do_it cmp'))
    in
    (10,run)

  let format_title =
    sprintf "%-7s %-7s"

  let display_numbers (num_runs,f) poly comp : string =
    let sum_poly = ref 0. in
    let sum_compare = ref 0. in
    for i = 1 to num_runs do
      let (time_polycmp,time_compare) =  (f poly comp) in
      sum_poly := !sum_poly +. time_polycmp;
      sum_compare := !sum_compare +. time_compare;
    done;
    let str = sprintf "%.4f   %.4f" !sum_poly !sum_compare in
    if !sum_poly < !sum_compare then
      sprintf "\027[31m%s\027[0m" str
    else if !sum_compare < !sum_poly /. 2.0 then
      sprintf "\027[32m%s\027[0m" str
    else
      str

  let all_tests () =
    printf "%s\n" T.description;
    print_endline "  compare ";
    let print_one (a,b) =
      printf "      %-25s: %s\n" a b
    in
    List.iter
      ~f:print_one
      [ ("test name", format_title "poly_cmp" "jane_compare");
        ("array of different values",   (display_numbers array_test compare T.compare));
        ("same values",  (display_numbers same_test compare T.compare));
        ("two different values",  (display_numbers different_test compare T.compare))]

end

module BI = Bench (Bench_int)
module BF = Bench (Bench_float)
module BFA = Bench (Bench_float_array)
module BFR = Bench (Bench_float_ref)
module BN = Bench (Bench_nan)
module BL = Bench (Bench_lambda)
module BR = Bench (Bench_record)
module BM = Bench (Bench_mix)
module BV = Bench (Bench_variant)
module BNI = Bench (Bench_nested)


let () =
  BI.all_tests ();
  BF.all_tests ();
  BFA.all_tests ();
  BFR.all_tests ();
  BN.all_tests ();
  BL.all_tests ();
  BR.all_tests ();
  BM.all_tests ();
  BV.all_tests ();
  BNI.all_tests ()
