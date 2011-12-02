open Core.Std
open Import

type job =
  { execution_context : Execution_context.t;
    work : unit -> unit Deferred.t;
  }

type t =
  { max_concurrent_jobs : int;
    continue_on_error : bool;
    mutable has_seen_error : bool;
    mutable num_running : int;
    waiting : job Queue.t;
  }

let is_dead t = t.has_seen_error && not t.continue_on_error

let invariant t =
  assert (0 <= t.num_running && t.num_running <= t.max_concurrent_jobs);
  if t.num_running < t.max_concurrent_jobs then assert (Queue.is_empty t.waiting);
  if not (Queue.is_empty t.waiting) then assert (t.num_running = t.max_concurrent_jobs);
  if is_dead t then assert (Queue.is_empty t.waiting);
;;

let create ~continue_on_error ~max_concurrent_jobs =
  { max_concurrent_jobs;
    continue_on_error;
    has_seen_error = false;
    num_running = 0;
    waiting = Queue.create ();
  }
;;

(* [really_run] logically takes up a "slot" in the throttle and runs items off of the
   queue in that slot until it can't find anything else to run.  The slot is "locked" by
   carefully incrementing and decrementing the [num_running] field in [t].  If a job
   throws an error before returning an Ok result the throttle will be marked as having
   seen an error, and may stop running jobs (based on ~continue_on_error passed to
   create).  If an asynchronous error is thrown after a job has completed successfully,
   the error will be silently ignored. *)
let rec really_run t job =
  t.num_running <- t.num_running + 1;
  assert (0 < t.num_running && t.num_running <= t.max_concurrent_jobs);
  match
    Scheduler.within_context job.execution_context (fun () -> Monitor.try_with job.work)
  with
  | Error () -> failwith "[try_with] unexpectedly raised"
  | Ok res ->
    res
    >>> fun res ->
    t.num_running <- t.num_running - 1;
    assert (0 <= t.num_running && t.num_running < t.max_concurrent_jobs);
    begin match res with
    | Ok () -> ()
    | Error exn ->
      t.has_seen_error <- true;
      Monitor.send_exn (Execution_context.monitor job.execution_context) exn
    end;
    if is_dead t then begin
      Queue.iter t.waiting ~f:(fun job ->
        Monitor.send_exn (Execution_context.monitor job.execution_context)
          (Failure "job aborted due to dead throttle"));
      Queue.clear t.waiting;
    end else
      match Queue.dequeue t.waiting with
      | None -> ()
      | Some job -> really_run t job
;;

let enqueue t f =
  if is_dead t then failwith "Throttle.enqueue into dead throttle";
  let execution_context = Scheduler.current_execution_context () in
  Deferred.create (fun i ->
    let work () = f () >>| fun a -> Ivar.fill i a in
    let job = { work; execution_context } in
    if t.num_running < t.max_concurrent_jobs then begin
      assert (Queue.is_empty t.waiting);
      really_run t job;
    end else
      Queue.enqueue t.waiting job)
;;

let is_empty t = (t.num_running = 0) && (Queue.is_empty t.waiting)

module Slot = struct
  type 'a t =
    { work : (unit -> 'a Deferred.t) Ivar.t;
      result : 'a Ivar.t;
    }

  let create () =
    { work = Ivar.create ();
      result = Ivar.create ();
    }
  ;;

  let run t work =
    Ivar.fill t.work work;
    Ivar.read t.result
  ;;

  let release t = whenever (
    run t (fun () -> Deferred.unit)
  )
end

let reserve_slot t =
  Deferred.create (fun i ->
    whenever (enqueue t (fun () ->
      let slot = Slot.create () in
      Ivar.fill i slot;
      Ivar.read slot.Slot.work
      >>= fun work ->
      work ()
      >>| fun result ->
      Ivar.fill slot.Slot.result result;
    )))
;;

let prior_jobs_done t =
  (* We queue t.max_concurrent_jobs dummy jobs and when they are all started we know that
     all prior jobs finished.  We make sure that all dummy jobs wait for the last one to
     get started before finishing. *)
  Deferred.create (fun all_dummy_jobs_running ->
    let dummy_jobs_running = ref 0 in
    for _i = 1 to t.max_concurrent_jobs do
      whenever (enqueue t (fun () ->
        incr dummy_jobs_running;
        if !dummy_jobs_running = t.max_concurrent_jobs then
          Ivar.fill all_dummy_jobs_running ();
        Ivar.read all_dummy_jobs_running))
    done)
;;

module Sequencer = struct
  type throttle = t
  type 'a t = { state : 'a; throttle : throttle }

  let create ?(continue_on_error=false) a = {
    state = a;
    throttle = create ~continue_on_error ~max_concurrent_jobs:1;
  }

  let enqueue t f = enqueue t.throttle (fun () -> f t.state)
end
