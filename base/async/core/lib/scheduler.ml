open Core.Std
open Import
open Deferred_std

module Execution_context = Basic.Execution_context
module Monitor = Raw_monitor

let debug = Debug.debug

include Basic.Scheduler

type 'a with_options = 'a Monitor.with_options

include struct
  open Monitor
  let schedule = schedule
  let schedule' = schedule'
  let with_execution_context = with_execution_context
  let within = within
  let within' = within'
  let within_context = within_context
  let within_v = within_v
end

let uncaught_exception () = t.uncaught_exception

let main_execution_context () = t.main_execution_context

let set_main_execution_context execution_context =
  t.main_execution_context <- execution_context;
;;

let set_block_group block_group =
  set_execution_context
    { current_execution_context () with Execution_context. block_group };
;;

let num_pending_jobs () = Jobs.length t.jobs

let next_upcoming_event () =
  match Events.next_upcoming t.events with
  | None -> None
  | Some events_event -> Some (Events.Event.at events_event)
;;

let cycle_start () = t.cycle_start

let cycle_times () = Tail.collect t.cycle_times

let cycle_num_jobs () = Tail.collect t.cycle_num_jobs

let cycle_count () = t.cycle_count

let jobs_left () = t.jobs_left

let num_jobs_run () = t.num_jobs_run

let set_max_num_jobs_per_priority_per_cycle int =
  if int <= 0 then
    fail "max_num_jobs_per_priority_per_cycle must be > 0" int <:sexp_of< int >>;
  t.max_num_jobs_per_priority_per_cycle <- int;
;;

let start_cycle ~now =
  if debug then Debug.print "start_cycle";
  t.cycle_count <- t.cycle_count + 1;
  t.cycle_start <- now;
;;

let finish_cycle =
  let rec do_batch jobs =
    if Option.is_none t.uncaught_exception then begin
      match jobs with
      | [] -> ()
      | job::jobs ->
        if debug then Debug.print "running job %d" (Job.id job);
        begin
          try
            t.num_jobs_run <- t.num_jobs_run + 1;
            Job.run job;
          with exn -> Monitor.send_exn (Monitor.current ()) exn ~backtrace:`Get;
        end;
        do_batch jobs
    end
  in
  let do_one_cycle () =
    Jobs.start_cycle t.jobs
      ~max_num_jobs_per_priority:t.max_num_jobs_per_priority_per_cycle;
    let rec loop t =
      match Jobs.get t.jobs with
      | [] -> ()
      | jobs -> do_batch jobs; loop t
    in
    loop t
  in
  fun ~now ->
    let num_jobs_run_before_cycle = t.num_jobs_run in
    begin match Events.advance_clock t.events ~to_:t.cycle_start with
    | `Not_in_the_future ->
      (* This could conceivably happen with NTP tweaking the clock.  There's no reason
         to do anything other than press on. *)
      ()
    | `Ok events -> List.iter events ~f:Clock_event.fire
    end;
    do_one_cycle ();
    t.jobs_left <- not (Jobs.is_empty t.jobs);
    if debug then
      Debug.log "cycle finished"
        (t.jobs_left, t.uncaught_exception, is_some (Events.next_upcoming t.events))
        <:sexp_of< bool * exn option * bool >>;
    (* This can potentially add more jobs if somebody is listening to cycle_times
       stream, so we have to check [Jobs.is_empty] before. *)
    Tail.extend t.cycle_times (Time.diff now t.cycle_start);
    Tail.extend t.cycle_num_jobs (t.num_jobs_run - num_jobs_run_before_cycle);
;;

