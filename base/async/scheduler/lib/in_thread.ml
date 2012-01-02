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

let run_holding_async_lock f =
  Raw_scheduler.run_in_async_gen
    (fun _ ->
      match Result.try_with f with
      | Error exn -> (`Do_not_run_a_cycle, Error exn)
      | Ok (maybe_run_a_cycle, a) -> (maybe_run_a_cycle, Ok a))
    Fn.id
;;

let run_in_async = Raw_scheduler.run_in_async

let run_in_async_exn f = Result.ok_exn (run_in_async f)

let run_in_async_wait = fun _ -> `Deprecated_use_Scheduler_block_on_async

let syscall f = run (fun () -> Syscall.syscall f)

let syscall_exn f = run (fun () -> Result.ok_exn (Syscall.syscall f))

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

let pipe () =
  let pipe_reader, pipe_writer = Pipe.create () in
  let put x =
    Raw_scheduler.run_in_async_wait_exn (fun () ->
      if Pipe.is_closed pipe_writer then
        failwith "put to pipe that has been closed"
      else
        Pipe.write pipe_writer x)
  in
  let close () = run_in_async_exn (fun () -> Pipe.close pipe_writer) in
  (pipe_reader, put, close)
;;

let am_in_async () = Raw_scheduler.am_in_async (the_one_and_only ())
