open Core.Std
(* this benchmarks three different operations versus their corresponding automatically
   generated ones: compare, (<), (=). There are three tests:

   - array of different_values : this produces an array of different values. The size of
   the array, called len, depends upon the type being checked. Every ith element is
   checked against the (len - i - 1)th element

   - structurally equal valuesvalues : this takes the same randomly generated value,
   structurally copies it, then compares it over and over again

   - physically equal values : this takes the same randomly generated value, physically copies it ,then compares
   it over and over again

   - two random values: this takes two random values and compares them.

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

module Bench_float : Benchable =
struct
  type t = float with compare
  let description = "float"
  let create_single () = Random.float 1000.
  let copy x = Obj.obj (Obj.dup (Obj.repr x))
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


module Bench_nested : Benchable =
struct
  type t = (((((((int * int) * int) * int) * int) * int) * int) * int) * int with compare,random
  let description = "int nested pairs"
  let copy (((((a,b),c),d),e),f) = (((((a,b),c),d),e),f)
  let create_single = random_of_t
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
    if flip = 0 then Var (String.copy "foo")
    else if flip = 1 then Lambda (String.copy "bar",create_random_lambda (size - 1))
    else App (create_random_lambda (size - 1), create_random_lambda (size - 1))

  let description = "lambda calculus"

  let rec copy =
    function
      | App(x,y) -> App(copy x, copy y)
      | Var x -> Var (String.copy x)
      | Lambda (x,t) ->
        Lambda (String.copy x,copy t)

  let sufficient_sample = 6

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
    let run cmp cmp'  =
      let length = Array.length arr in
      let do_it cmp () =
        for j = 1 to 1000 do
          for i = 0 to length - 1 do
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
    let run cmp cmp'   =
      let do_it cmp () =
        for i = 0 to T.sufficient_sample * 1000 do
          ignore (cmp x x)
        done
      in
      (time (do_it cmp), time (do_it cmp'))
    in
    (10, run)

  let copy_test =
    let x = T.create_single () in
    let y = T.copy x in
    let run cmp cmp'   =
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

  let format_numbers =
    sprintf "%.4f   %.4f   %.4f%%   %.4f   %.4f"


  let format_title =
    sprintf "%-7s %-7s %-7s %-7s %-7s"


  let t_less a b =
    if T.compare a b < 0 then true else false

  let t_eq a b = if T.compare a b = 0 then true else false



  let display_numbers (num_runs,f) poly mono =
    let square x = x *. x in
    let arr = Array.create num_runs (0.,0.) in
    let sum_poly = ref 0. in
    let sum_mono = ref 0. in
    for i = 1 to num_runs do
      let (time_polycmp,time_cmp) =  (f poly mono) in
      Array.set arr (i-1) (time_polycmp, time_cmp);
      sum_poly := !sum_poly +. time_polycmp;
      sum_mono := !sum_mono +. time_cmp;
    done;
    let mean_poly = !sum_poly /. (float_of_int num_runs) in
    let mean_mono = !sum_mono /. (float_of_int num_runs) in
    let std_dev_poly = ref 0. in
    let std_dev_mono = ref 0. in
    for i = 0 to num_runs -1 do
      let (poly_measurement,mono_measurement) = Array.get arr i  in
      std_dev_poly := !std_dev_poly +. (square (poly_measurement -. mean_poly));
      std_dev_mono := !std_dev_mono +. (square (mono_measurement -. mean_mono))
    done;
    let percent = (!sum_poly -. !sum_mono) /. !sum_mono in
    format_numbers
      !sum_poly !sum_mono  (percent *. 100.0)
      (sqrt !std_dev_poly)  (sqrt !std_dev_mono)


  let all_tests () =
    printf "%s\n" T.description;
    print_endline "  compare ";
    let print_one (a,b) =
      printf "      %-30s: %s\n" a b
    in
    List.iter
      ~f:print_one
      [ ("test name", format_title "poly_compare" "jane_compare" "diff%" "sdev_poly" "sdev_jane");
        ("array of random values",   (display_numbers array_test) compare T.compare);
        ("two physically equal values",  (display_numbers same_test compare T.compare));
        ("two structurally equal values",  (display_numbers copy_test compare T.compare));
        ("two random values",  (display_numbers different_test compare T.compare))];
    print_endline "  (<) ";
    List.iter
      ~f:print_one
      [ ("test name", format_title "poly_compare" "jane_compare" "diff%" "sdev_poly" "sdev_jane");
        ("array of random values",   (Obj.magic (display_numbers array_test)) (<) t_less);
        ("two physically equal values",  (Obj.magic (display_numbers same_test)) (<)
    t_less);
        ("two structurally equal values",  (Obj.magic (display_numbers copy_test)) (<) t_less);
        ("two random values",  (Obj.magic (display_numbers different_test)) (<) t_less)];
    print_endline "  (=) ";
    List.iter
      ~f:print_one
      [ ("test name", format_title "poly_compare" "jane_compare" "diff%" "sdev_poly" "sdev_jane");
        ("array of random values",   (Obj.magic (display_numbers array_test)) (=) t_eq);
        ("two physically equal values",  (Obj.magic (display_numbers same_test)) (=) t_eq);
        ("two structurally equal values",  (Obj.magic (display_numbers copy_test)) (=) t_eq);
        ("two random values",  (Obj.magic (display_numbers different_test)) (=) t_eq)]



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
