open Core.Std
open Import
open Deferred_std

module Internal_job : sig
  type t with sexp_of

  val create : (unit -> unit Deferred.t) -> t
  val start_exn : t -> (unit, unit) Result.t Deferred.t
  val started : t -> unit Deferred.t
  val abort : t -> unit
end  = struct
  type t =
    { work : unit -> unit Deferred.t;
      started : unit Ivar.t;
      execution_context : Execution_context.t;
    }
  with sexp_of

  let started t = Ivar.read t.started

  let create work =
    { work;
      started = Ivar.create ();
      execution_context = Scheduler.current_execution_context ();
    }

  let send_exn t exn =
    Monitor.send_exn (Execution_context.monitor t.execution_context) exn
  ;;

  let abort t = send_exn t (Failure "job aborted due to dead throttle")

  let start_exn t =
    if Ivar.is_full t.started then failwith "Job.start_exn called twice on the same job";
    Ivar.fill t.started ();
    match
      Scheduler.within_context t.execution_context (fun () -> Monitor.try_with t.work)
    with
    | Error () -> failwith "[Job.run] unexpectedly raised"
    | Ok res ->
      res
      >>| function
      | Ok () -> Ok ()
      | Error exn -> send_exn t exn; Error ()
  ;;
end

type t =
  { max_concurrent_jobs : int;
    continue_on_error : bool;
    job_reader : Internal_job.t Pipe.Reader.t;
    job_writer : Internal_job.t Pipe.Writer.t;
  }
with fields, sexp_of

let invariant t =
  Pipe.Reader.invariant t.job_reader;
  Pipe.Writer.invariant t.job_writer;
;;

let kill t =
  begin match Pipe.read_now t.job_reader with
  | `Eof | `Nothing_available -> ()
  | `Ok q -> Queue.iter q ~f:Internal_job.abort
  end;
  Pipe.close_read t.job_reader;
;;

let one_job_runner t =
  whenever (Deferred.repeat_until_finished () (fun () ->
    Pipe.read t.job_reader
    >>= function
    | `Eof -> return (`Finished ())
    | `Ok job ->
      Internal_job.start_exn job
      >>| function
      | Ok () -> `Repeat ()
      | Error () ->
        if t.continue_on_error then
          `Repeat ()
        else
          (kill t; `Finished ())))
;;

let create ~continue_on_error ~max_concurrent_jobs =
  let (job_reader, job_writer) = Pipe.create () in
  let t =
    { max_concurrent_jobs;
      continue_on_error;
      job_reader;
      job_writer;
    }
  in
  for i = 1 to max_concurrent_jobs; do
    one_job_runner t;
  done;
  t
;;

module Job = struct
  type 'a t =
    { internal_job : Internal_job.t;
      result : 'a Ivar.t;
    }

  let result t = Ivar.read t.result
  let started t = Internal_job.started t.internal_job

  let create f =
    let result = Ivar.create () in
    let internal_job =
      Internal_job.create (fun () -> f () >>| fun a -> Ivar.fill result a)
    in
    { internal_job; result }
  ;;
end

let enqueue_job t job = whenever (Pipe.write t.job_writer job.Job.internal_job)

let enqueue t f =
  let job = Job.create f in
  enqueue_job t job;
  Job.result job;
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
