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



module Test_int =
struct
  type t = int with compare


  let time f =
    let start = Unix.gettimeofday () in
  f ();
    let finish = Unix.gettimeofday () in
  finish -. start
  ;;

  let run () =
  let a = 42 in
  let b = 46 in
  let f () =
    for i = 1 to 200000000 do
      ignore(compare a b)
    done
  in
  let g () =
    for i = 1 to 200000000 do
      ignore(compare (a:int) (b:int))
    done
  in
  let time_f = time f in
  let time_g = time g in
  printf "%f %f\n"
    time_f
    time_g
end

module Test_float =
struct
  type t = float with compare

  let time f =
    let start = Unix.gettimeofday () in
  f ();
    let finish = Unix.gettimeofday () in
  finish -. start
  ;;

  let run () =
  let a = 42. in
  let b = 46. in
  let f () =
    for i = 1 to 200000000 do
      ignore(compare a b)
    done
  in
  let g () =
    for i = 1 to 200000000 do
      ignore(compare (a:float) (b:float))
    done
  in
  let time_f = time f in
  let time_g = time g in
  printf "%f %f\n"
    time_f
    time_g
end


let _ =
  Test_int.run ();
  Test_float.run ()
