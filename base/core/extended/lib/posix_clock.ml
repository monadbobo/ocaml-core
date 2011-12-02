type t = 
  | Realtime
  | Monotonic
  | Process_cpu
  | Process_thread

let all = [
  Realtime;
  Monotonic;
  Process_cpu;
  Process_thread;
]

let to_string t =
  match t with
  | Realtime       -> "Realtime"
  | Monotonic      -> "Monotonic"
  | Process_cpu    -> "Process_cpu"
  | Process_thread -> "Process_thread"

external getres : t -> int = "caml_clock_getres"
external gettime : t -> int = "caml_clock_gettime"
(*external nanosleep : t -> int -> unit = "caml_clock_nanosleep"*)

let min_interval t =
  let canary_val = 1_000_000 in
  let current_min = ref canary_val in
  for i = 1 to 10_000 do
    let t1 = gettime t in
    let t2 = gettime t in
    if t1 <> t2 && t2 > t1 then current_min := min (t2 - t1) !current_min
  done;

  if !current_min <> canary_val then !current_min
  else failwith (Printf.sprintf "unable to calculate min_interval for %s" (to_string t))

let mean_gettime_cost ~measure ~using =
  assert (getres Process_cpu = 1);
  let count = 10_000_000 in
  let start = gettime using in
  for i = 1 to count do
    ignore (gettime measure);
  done;
  let stop = gettime using in
  (stop - start) / count
