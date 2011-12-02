open Core.Std

let verbosity_threshold = ref `Low
let cmp_verbosity a b =
  match a, b with
  | `Low, `Low -> 0
  | `Low, _ -> -1
  | `Mid, `Low -> 1
  | `Mid, `Mid -> 0
  | `Mid, `High -> -1
  | `High, `High -> 0
  | `High, _ -> 1
;;

let printout msg_verbosity =
  if cmp_verbosity !verbosity_threshold msg_verbosity >= 0
  then printf else ifprintf stdout
;;

let default_clock = `Wall

module Test = struct
  type t =
    { name : string option;
      size : int option;
      func : unit -> unit;
    }
  ;;

  let create ?name ?size func = { name; size; func }

  let name t = t.name
  let size t = t.size
end

module Result = struct
  module Stat = struct
    type t = {
      run_time : int;
      gc_time : int;
      sample_size : int;
      compactions : int;
      allocated : int;
    }

    let empty = {
      run_time = 0;
      gc_time  = 0;
      sample_size = 0;
      compactions = 0;
      allocated = 0;
    }
  end
  open Stat

  type t = string option * int option * Stat.t array

  let fold arr f init =
    Array.fold_right arr ~init ~f:(fun r v ->
      { run_time = f r.run_time v.run_time;
        gc_time  = f r.gc_time v.gc_time;
        sample_size = f r.sample_size v.sample_size;
        compactions = f r.compactions v.compactions;
        allocated = f r.allocated v.allocated;
      })

  let mean arr =
    let sum = fold arr (+) empty in
    { run_time = sum.run_time / Array.length arr;
      gc_time  = sum.gc_time / Array.length arr;
      sample_size = sum.sample_size / Array.length arr;
      compactions = sum.compactions / Array.length arr;
      allocated = sum.allocated / Array.length arr;
    }

  let min arr = fold arr min arr.(0)
  let max arr = fold arr max arr.(0)

  let sample_size arr = arr.(0).sample_size

  let sdev arr =
    let mean_run = (mean arr).run_time in
    let diff_sq x y =
      let d = (Float.of_int x) -. (Float.of_int y) in
      d *. d
    in
    let squares = Array.map arr ~f:(fun stat -> diff_sq mean_run stat.run_time) in
    let squares_sum = Array.fold squares ~init:0. ~f:(+.) in
    let init_sd = sqrt (squares_sum /. Float.of_int (Array.length arr)) in
    Int.of_float (init_sd /. sqrt (Float.of_int (sample_size arr)))

  let compactions_occurred arr = (max arr).compactions > 0

  let allocated_varied arr =
    (max arr).allocated <> (min arr).allocated
end

(* printing functions *)

let rec time_string ~time_format n =
  let auto_format n =
    if n < 1_000_000 then `Ns
    else if n < 1_000_000_000 then `Us
    else if n < 1_000_000_000_000 then `Ms
    else `S
  in
  match time_format with
  | `Auto -> time_string ~time_format:(auto_format n) n
  | `Ns -> Int.to_string n ^ " ns"
  | `Us -> Int.to_string (n / 1000) ^ " us"
  | `Ms -> Int.to_string (n / 1_000_000) ^ " ms"
  | `S  -> Int.to_string (n / 1_000_000_000) ^ " s"
;;

let make_name (name_opt, _size_opt, _results) =
  match name_opt with
  | Some name -> name
  | None -> ""
;;

let make_size (_name_opt, size_opt, _results) =
  match size_opt with
  | Some size -> Int.to_string size
  | None -> ""
;;

let make_time ~time_format (_name_opt, _size_opt, results) =
  time_string ~time_format ((Result.mean results).Result.Stat.run_time)
;;

let make_norm ~time_format (_name_opt, size_opt, results) =
  match size_opt with
  | Some size ->
    if size > 0 then
      time_string ~time_format ((Result.mean results).Result.Stat.run_time / size)
    else
      ""
  | None -> ""
;;

let make_sdev ~time_format (_name_opt, _size_opt, results) =
  time_string ~time_format (Result.sdev results)
;;

let make_allocated (_name_opt, _size_opt, results) =
  Int.to_string (Result.mean results).Result.Stat.allocated
;;

let make_warn (_name_opt, _size_opt, results) =
  let maybe_string s predicate = if predicate then s else "" in
  let open Result in
  let mean_run = (mean results).Stat.run_time in
  let min_run  = (min  results).Stat.run_time in
  let max_run  = (max  results).Stat.run_time in
  maybe_string   "m" ((mean_run - min_run) > mean_run / 20)
  ^ maybe_string "M" ((max_run - mean_run) > mean_run / 20)
  ^ maybe_string "c" (compactions_occurred results)
  ^ maybe_string "a" (allocated_varied results)
;;

let print ?(time_format=`Auto) data =
  let r_create = Ascii_table.Column.create ~align:Ascii_table.Align.right in
  let name_col = r_create "Name" make_name in
  let size_col = r_create "Input size" make_size in
  let time_col = r_create "Run time" (make_time ~time_format) in
  let norm_col = r_create "Normalized" (make_norm ~time_format) in
  let sdev_col = r_create "S. dev." (make_sdev ~time_format) in
  let allocated_col = r_create "Allocated" make_allocated in
  let warn_col = r_create "Warnings" make_warn in

  let exists_name = List.exists data ~f:(fun (name_opt, _, _) -> is_some name_opt) in
  let exists_size = List.exists data ~f:(fun (_, size_opt, _) -> is_some size_opt) in
  Ascii_table.output ~oc:stdout
    begin
      List.concat [
        (if exists_name then [name_col] else []);
        (if exists_size then [size_col] else []);
        [time_col];
        (if exists_size then [norm_col] else []);
        [sdev_col; allocated_col; warn_col]
      ]
    end
    data
;;

(* end printing functions *)

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.Stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.Stat.live_words
  in
  loop 10 0

let full_major_cost ~now () =
  let count = 10 in
  let s = now () in
  for i = 1 to count do
    Gc.full_major ();
  done;
  let e = now () in
  (e - s) / count

let find_run_size ~now gettime_cost f =
  let rec loop samples =
    let s = now () in
    for i = 1 to samples do
      f ();
    done;
    let e = now () in
    (* we need enough samples so that the gettime_cost is < 1% of the cost of the run
       and we also demand that the total run take at least .5 seconds *)
    if gettime_cost > ((e - s) / 100) || (e - s) < 50 * 1000 * 1000
    then loop (samples * 2)
    else (samples, e - s)
  in
  loop 1

let gc_allocated gc_stat =
  let v =
    gc_stat.Gc.Stat.major_words
    +. gc_stat.Gc.Stat.minor_words
    -. gc_stat.Gc.Stat.promoted_words
  in
  Int.of_float v

let run_once ~f ~sample_size ~gettime_cost ~full_major_cost ~allocated_cost ~now =
  let stat_s = Gc.quick_stat () in
  let run_s = now () in
  for i = 1 to sample_size do
    f ();
  done;
  let run_e = now () in
  let stat_m = Gc.quick_stat () in
  Gc.full_major ();
  let gc_e = now () in
  let stat_e = Gc.quick_stat () in
  {Result.Stat.
    run_time = (run_e - run_s - gettime_cost) / sample_size;
    gc_time  = (gc_e - run_e - full_major_cost) / sample_size;
    sample_size;
    compactions = stat_e.Gc.Stat.compactions - stat_s.Gc.Stat.compactions;
    allocated = ((gc_allocated stat_m) - (gc_allocated stat_s) - allocated_cost)
                / sample_size;
  }

let allocated_cost ~now () =
  let f () = () in
  let compute ?(allocated_cost=0) sample_size=
    let result =
      run_once ~f ~sample_size ~gettime_cost:0 ~full_major_cost:0
        ~allocated_cost ~now
    in
    result.Result.Stat.allocated
  in
  let allocated_cost = compute 1 in
  assert (0 = compute ~allocated_cost 1);
  assert (0 = compute ~allocated_cost 100);
  allocated_cost

let parse_clock maybe_clock =
  let clock = Option.value maybe_clock ~default:default_clock in
  match clock with
  | `Wall -> Posix_clock.Monotonic
  | `Cpu -> Posix_clock.Process_cpu

let bench_basic ~gc_prefs ~no_compactions ?clock ~run_count {Test.name; func = f} =
  let old_gc = Gc.get () in
  begin match gc_prefs with
    | Some prefs -> Gc.set prefs
    | None -> ()
  end;
  let measurement_clock = parse_clock clock in
  let now () = Posix_clock.gettime measurement_clock in
  if no_compactions then Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };
  (* calculate how long it takes us to get a time measurement for the current thread *)
  printout `High "calculating cost of timing measurement: %!";
  let gettime_cost =
    Posix_clock.mean_gettime_cost ~measure:measurement_clock ~using:Posix_clock.Monotonic
  in
  printout `High "%i ns\n%!" gettime_cost;
  printout `High "calculating minimal measurable interval: %!";
  let gettime_min_interval = Posix_clock.min_interval measurement_clock in
  printout `High "%i ns\n%!" gettime_min_interval;
  (* find the number of samples of f needed before gettime cost is < 1% of the total *)
  printout `High "determining number of runs per sample: %!";
  let sample_size, run_time = find_run_size ~now gettime_min_interval f in
  printout `High "%i\n%!" sample_size;
  let runs = Array.create run_count Result.Stat.empty in
  printout `High "stabilizing GC: %!";
  stabilize_gc ();
  printout `High "done\n%!";
  printout `High "calculating the cost of a full major sweep: %!";
  let full_major_cost = full_major_cost ~now () in
  printout `High "%i ns\n%!" full_major_cost;
  printout `High "calculating memory allocated by tester: %!";
  let allocated_cost = allocated_cost ~now () in
  printout `High "%i words\n%!" allocated_cost;
  printout `Mid "running samples for %s (estimated time %i sec)\n%!"
    (Option.value ~default:"(NO NAME)" name)
    ((run_time * run_count) / 1000 / 1000 / 1000);
  for i = 0 to run_count - 1 do
    runs.(i) <- run_once ~f ~sample_size ~gettime_cost ~full_major_cost ~allocated_cost
      ~now;
    printout `Mid ".%!";
  done;
  printout `Mid "\n%!";
  (* keep f from being gc'd by calling f () again *)
  f ();
  Gc.set old_gc;
  runs

type ('a, 'b) benchmark_function =
  ?verbosity:[ `High | `Mid | `Low ] -> ?gc_prefs:Gc.Control.t -> ?no_compactions:bool
  -> ?fast:bool -> ?clock:[`Wall | `Cpu] -> 'a -> 'b

type 'a print_function =
  ?time_format:[`Ns | `Ms | `Us | `S | `Auto]
  -> 'a

let default_run_count = 100

let bench_raw ?verbosity ?gc_prefs ?(no_compactions = false) ?fast ?clock tests =
  let run_count = if Option.value ~default:false fast then 1 else default_run_count in
  verbosity_threshold := Option.value ~default:`Low verbosity;
  List.map tests ~f:(fun test -> test.Test.name, test.Test.size,
    bench_basic ~gc_prefs ~no_compactions ?clock ~run_count test)
;;

let bench ?time_format ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests =
  print ?time_format (bench_raw ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests)
;;

module Bundle = struct
  type 'a t =
    Base of 'a
  | App of Test.t list * (unit -> 'a) * 'a

  let create init = Base init

  let (>>|) bundle f =
    match bundle with
    | Base init -> App
      ( [Test.create ~name:"#1" (fun () -> ignore (f init))],
        (fun () -> f init),
        f init
      )
    | App (separate, composed, init) -> App
      ( (Test.create ~name:(sprintf "#%d" (List.length separate + 1))
          (fun () -> ignore (f init))) :: separate,
        (fun () -> f (composed ())),
        f init
      )

  (* this does not account for the time spent applying thunks to () but should *)
  let bench bundle =
    match bundle with
    | Base _ -> printf "Bundle is fully evaluated."
    | App (separate, composed, _) ->
      let composed_test = Test.create ~name:"Composed" (Fn.compose ignore composed) in
      bench (composed_test :: separate)
end
