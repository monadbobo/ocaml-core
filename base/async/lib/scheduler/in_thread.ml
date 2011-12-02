open Core.Std
open Import

let the_one_and_only = Raw_scheduler.the_one_and_only

module Helper_thread = struct
  include Raw_scheduler.Helper_thread

  let create ?name_first16 () =
    Raw_scheduler.create_helper_thread (the_one_and_only ()) ?name_first16
  ;;
end

let deferred () = Raw_scheduler.thread_safe_deferred (the_one_and_only ())

let pipe ?max_batch ?allow_straight_through () =
  Raw_scheduler.thread_safe_pipe (the_one_and_only ()) ?max_batch ?allow_straight_through
;;

let run ?thread ?name_first16 f =
  let scheduler = the_one_and_only () in
  if scheduler.Raw_scheduler.go_has_been_called then
    Raw_scheduler.run_in_thread scheduler ?thread ?name_first16 f
  else
    (* We use [bind unit ...] to force calls to [run_in_thread] to wait until after the
       scheduler is started.  We do this because [run_in_thread] will cause things to run
       in other threads, and when a job is finished in another thread, it will try to
       acquire the async lock and manipulate async datastructures.  This seems hard to
       think about if async hasn't even started yet. *)
    Deferred.bind
      Deferred.unit
      (fun () -> Raw_scheduler.run_in_thread scheduler ?thread ?name_first16 f)
;;

let run_in_async = Raw_scheduler.run_in_async

let run_in_async_exn f = Result.raise_error (run_in_async f)

let run_in_async_wait = fun _ -> `Deprecated_use_Scheduler_block_on_async


let syscall f = run (fun () -> Syscall.syscall f)

let syscall_exn f = run (fun () -> Result.raise_error (Syscall.syscall f))

let pipe_of_squeue sq =
  (* All the little definitions are to avoid unessary allocation, since it's possible this
     guy might be used to do something like consume a quotefeed. *)
  let (r, w) = Pipe.create () in
  let pull () =
    let q = Queue.create () in
    Squeue.transfer_queue sq q;
    q
  in
  let rec continue q = Pipe.write' w q >>> loop
  (* [run pull] runs [pull] in a thread, because [Squeue.transfer_queue] can block. *)
  and loop () = run pull >>> continue in
  loop ();
  r
;;

let am_in_async () = Raw_scheduler.am_in_async (the_one_and_only ())
