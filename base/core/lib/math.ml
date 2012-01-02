(* Implements the Kahan summation:
   http://en.wikipedia.org/wiki/Kahan_summation_algorithm
   It has some good properties, but still doesn't do as well as Shewchuk's algorithm,
   for example sum [1.; 1e100; 1.; -1e100] yields 0.0.
*)
let sum t =
  let accum = ref 0. in
  let compensation = ref 0. in
  for i = 0 to (Array.length t) - 1 do
    let y = t.(i) -. !compensation in
    let new_accum = !accum +. y in
    compensation := (new_accum -. !accum) -. y;
    accum := new_accum
  done;
  !accum


module List_math = struct
  (** [linspace] is similar to [frange], but it takes the number of elements in the output
      as an argument, rather than the size of the stride, which is more numerically robust.
      The [endpoint] parameter explicitly controls whether [stop] value should be included
      in the output (the default) or not.

      This function is a clone of [numpy.linspace].
  *)
  let linspace ?(endpoint=true) start stop num =
    let num2 = if endpoint then (num - 1) else num in
    if num2 < 1 then
      invalid_arg "Math.linspace: num too small";
    let stride = (stop -. start) /. float_of_int num2 in
    let is_nan (x : float) = x <> x in
    if is_nan stride then
      invalid_arg "Math.linspace: got nans";
    let rec loop counter accum =
      if counter < 0 then accum
      else
        loop (counter - 1) ((start +. (float_of_int counter) *. stride)::accum)
    in
    loop (num2 - 1) (if endpoint then [stop] else [])

  (* For unit tests *)
  let rec float_lists_almost_equal l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | h1::t1, h2::t2 ->
      if abs_float (h1 -. h2) < (abs_float h1 +. abs_float h2) *. 1e-16 then
        float_lists_almost_equal t1 t2
      else
        false
    | _h::_t, []
    | [], _h::_t -> false


  TEST = float_lists_almost_equal (linspace 0.3 0.9 5) [0.3; 0.45; 0.6; 0.75; 0.9]
  TEST = float_lists_almost_equal
    (linspace ~endpoint:false 0.3 0.9 5)[0.3; 0.42; 0.54; 0.66; 0.78]

  (* The tests below are only using integers, so the result should be exact. *)
  TEST = linspace 3. 8. 6 = [3.; 4.; 5.; 6.; 7.; 8.]
  TEST = linspace ~endpoint:false 3. 8. 5 = [3.; 4.; 5.; 6.; 7.]
  TEST = linspace 3. 8. 2 = [3.; 8.]
  TEST = linspace ~endpoint:false 3. 8. 1 = [3.]
  TEST = linspace 3. (-7.) 6 = [3.; 1.; -1.; -3.; -5.; -7.]
  TEST = linspace (-3.) 7. 2 = [-3.; 7.]

  (** [frange] is similar to [range], but for floats. *)
  let frange ?(stride=1.) start stop =
    if 0. = stride then
      invalid_arg "Core_list.frange: stride must be non-zero";
    (* Generate the range starting at the last element, so that we do not need to rev it *)
    let rec loop counter accum =
      if counter <= 0.0 then accum
      else
        (* The following is a floating-point subtraction, but [counter] is a float
           representing an exact integer value between 0 and 5e8.  So the operation is
           guaranteed by IEEE to give no rounding errors. *)
        let counter_minus_1 = counter -. 1.0 in
        (* Using [first +. counter *. stride] is much more accurate then adding or
           subtracting stride at each step. *)
        loop counter_minus_1 ((start +. counter_minus_1 *. stride)::accum)
    in
    let strides_from_start_to_stop = (stop -. start) /. stride in
    let is_nan x = (x : float) <> x in
    if is_nan strides_from_start_to_stop then
      (* We know that [stride] is non-zero, so some arguments must have been [nan] *)
      invalid_arg "Core_list.frange: got nans";
    if strides_from_start_to_stop > 5e8 then
      (* This protects us from [stride] too close to 0.0, infinite [start] or [stop] etc. *)
      invalid_arg "Core_list.frange: result too long";
    if strides_from_start_to_stop <= 0.0 then
      []
    else
      (* If [stop -. start] is epsilon-close to an exact multiple of [stride], we assume
         that the caller's intention was for it to be the exact multiple, so that the last
         element is [stop -. stride].  E.g. we have

         For IEEE doubles: (0.9 -. 0.3) /. 0.3 = 2.0000000000000004 > 2.0

         but thanks to the use of epsilon below, [frange ~stride:0.3 0.3 0.9] will still
         return a 2-element list:

         [0.3; 0.6]

         while [~stride:0.3 0.3 0.9000001] returns something within a couple of ulps from

         [0.3; 0.6; 0.9]
      *)
      let epsilon = 1e-7 in
      let num_elts = 1.0 +. floor (strides_from_start_to_stop -. epsilon) in
      loop num_elts []


  (* For IEEE doubles: (0.9 -. 0.3) /. 0.3 = 2.0000000000000004 > 2.0 *)
  TEST = float_lists_almost_equal (frange ~stride:0.3 0.3 0.8999999) [0.3; 0.6]
  TEST = float_lists_almost_equal (frange ~stride:0.3 0.3 0.9000000) [0.3; 0.6]
  TEST = float_lists_almost_equal (frange ~stride:0.3 0.3 0.9000001) [0.3; 0.6; 0.9]

  (* For IEEE doubles: (0.3 -. 0.1 /. 0.1 = 1.9999999999999998 < 2.0 *)
  TEST = float_lists_almost_equal (frange ~stride:0.1 0.1 0.2999999) [0.1; 0.2]
  TEST = float_lists_almost_equal (frange ~stride:0.1 0.1 0.3000000) [0.1; 0.2]
  TEST = float_lists_almost_equal (frange ~stride:0.1 0.1 0.3000001) [0.1; 0.2; 0.3]

  (* The tests below are only using integers, so the result should be exact. *)
  TEST = frange 3. 1.  = []
  TEST = frange 3. 3.  = []
  TEST = frange 3. 3.1 = [3.]
  TEST = frange 3. 8.  = [3.; 4.; 5.; 6.; 7.]
  TEST = frange ~stride:3. 4. 10. = [4.; 7.]
  TEST = frange ~stride:3. 4. 11. = [4.; 7.; 10.]
  TEST = frange ~stride:3. 4. 12. = [4.; 7.; 10.]
  TEST = frange ~stride:3. 4. 13. = [4.; 7.; 10.]
  TEST = frange ~stride:3. 4. 14. = [4.; 7.; 10.; 13.]

  TEST = frange ~stride:(-1.)  1. 3.  = []
  TEST = frange ~stride:(-1.)  3. 3.  = []
  TEST = frange ~stride:(-1.)  4. 3.9 = [4.]
  TEST = frange ~stride:(-1.)  8. 3.  = [8.; 7.; 6.; 5.; 4.]
  TEST = frange ~stride:(-3.) 10. 4.  = [10.; 7.]
  TEST = frange ~stride:(-3.) 10. 3.  = [10.; 7.; 4.]
  TEST = frange ~stride:(-3.) 10. 2.  = [10.; 7.; 4.]
  TEST = frange ~stride:(-3.) 10. 1.  = [10.; 7.; 4.]
  TEST = frange ~stride:(-3.) 10. 0.  = [10.; 7.; 4.; 1.]

  module Test_values = struct
    let flong1 =
      let v = lazy (linspace 0. 2000. 200_001) in
      fun () -> Lazy.force v

    let flong2 =
      let v = lazy (linspace 0.01 0.01 100_000) in
      fun () -> Lazy.force v

    let l1 = [1;2;3;4;5;6;7;8;9;10]
  end

  let sum t = sum (Core_array.of_list t)
  let sum_sq t = sum (Core_list.map t ~f:(fun x -> x *. x))
  let sum_product_exn s t = sum (Core_list.map2_exn s t ~f:( *. ))

  let naive_sum = Core_list.fold ~init:0.0 ~f:(+.)
  let sum_vs_naive_sum_diff t = abs_float(naive_sum t -. sum t)
  TEST = sum (Test_values.flong1 ()) = 200001000.0
  TEST = sum_vs_naive_sum_diff (Test_values.flong1 ()) > 2e-8
  TEST = sum (Test_values.flong2 ()) = 1000.0
  TEST = sum_vs_naive_sum_diff (Test_values.flong2 ()) > 7e-10

end


module Array_math = struct
  let sum = sum
  let sum_sq t = sum (Core_array.map t ~f:(fun x -> x *. x))
  let sum_product_exn s t = sum (Core_array.map2 s t ~f:( *. ))
  let linspace ?(endpoint=true) start stop num =
    Core_array.of_list (List_math.linspace ~endpoint start stop num)
  let frange ?(stride=1.) start stop =
    Core_array.of_list (List_math.frange ~stride start stop)
end
