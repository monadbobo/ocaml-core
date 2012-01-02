(** The In_thread module has functions for interaction between the Async world
    and other (kernel) threads.  The name is to remind us to think about threads
    and race conditions. *)

open Core.Std
open Async_core

module Helper_thread : sig
  (** A Helper_thread is a thread that is dedicated to handling computations
      external to Async.  We need them because some libraries (e.g. Sqlite3)
      require that certain collections of computations run in the same thread.
  *)
  type t

  (* [create ?name_first16 ()] creates a new helper thread.  The first 16 chars
     of [name_first16] will be used as the thread name for any work that that
     is done by the thread that doesn't get its own name. *)
  val create : ?name_first16:string -> unit -> [ `Ok of t | `Out_of_threads ]
end

(** [pipe ()] returns [(reader, put, close)], where [reader] receives all of the values
    supplied to [put] and is closed when [close] is called.

    [put] and [close] must be called from threads other than async.

    [put] will block (i.e. pushback) based on [reader]'s pushback.

    It is ok to call [close] multiple times.  Only the first will have an effect.

    Calling [put] after [close] will raise. *)
val pipe : unit -> 'a Pipe.Reader.t * ('a -> unit) * (unit -> unit)

(** [pipe_of_squeue squeue] returns a pipe [p] and consumes the contents [squeue], placing
    them in [p].  It repeatedly grabs everything from [squeue], places it in [p], and
    then waits for pushback on [p]. *)
val pipe_of_squeue : 'a Squeue.t -> 'a Pipe.Reader.t

(** [deferred ()] returns [(d, put)] where [d] is a deferred that will become determined
    with value [v] once [put v] is called.  It is safe to call [put] from another
    thread. *)
val deferred : unit -> 'a Deferred.t * ('a -> unit)

(** [run ?thread ?name_first16 f] runs [f()] in another thread and returns
    the result as a Deferred in the Async world.  If [f()] raises an exception
    (asynchronously, since it is another thread) then that exception will be
    raised to the monitor that called [run()].

    Async code should not be used from within [f].

    If [thread] is not supplied, then any thread from the thread pool could be
    used.  If you need to run routines in a specific thread (as is required by
    some libraries like Sqlite), you should create a helper thread and supply it
    to [run].

    If you call [run] several times with the same helper thread, the [f()] calls
    will run in sequence, in the order in which they are supplied to [run].
    Each [f()] will complete (return or raise) before another [f()] starts.

    For example, if you call

      [run ~thread f1]
      [run ~thread f2]
      [run ~thread f3]

    Then the thread will run [f1()] to completion, then [f2()] to completion,
    then [f3()] to completion.

    If [name_first16] is supplied, the name of the thread will be set to it
    for the duration of the execution of [f ()].
*)
val run :
  ?thread:Helper_thread.t
  -> ?name_first16:string
  -> (unit -> 'a)
  -> 'a Deferred.t

(** [run_holding_async_lock f] acquires the async lock and runs [f ()] while holding the
    lock.  Depending on the result of [f], it may also run a cycle. *)
val run_holding_async_lock
  :  (unit -> [ `Run_a_cycle | `Do_not_run_a_cycle ] * 'a)
  -> ('a, exn) Result.t

(** [run_in_async f] acquires the async lock and runs [f ()] and an async cycle while
    holding the lock.  It returns the result of [f ()] to the outside world.  The caller
    is blocked until the cycle is complete.  If [f] raises an exception then
    [run_in_async] will raise an exception.

    [run_in_async] does not automatically start the async scheduler.  You still need to
    call either [Scheduler.go] or [Scheduler.go_in_thread] elsewhere in your program.

    [run_in_async] runs an async cycle in its thread to give good latency -- we
    immediately run the async code that depends on [f ()] without needing another thread.
*)
val run_in_async     : (unit -> 'a) -> ('a, exn) Result.t
val run_in_async_exn : (unit -> 'a) ->  'a

(** [run_in_async_wait f] is like [run_in_async] except that [f] returns a deferred, and
    so the call to [run_in_async_wait] blocks until that deferred becomes determined.  If
    [f] or any job spawned by [f] raises an exception then [run_in_async_wait] will raise
    an exception.

    [run_in_async_wait] is much slower than [run_in_async] if it has to block, because
    doing so requires creating an squeue, which contains a mutex and condition variables,
    and the blocking on [Squeue.pop]. *)
val run_in_async_wait
  :  (unit -> 'a Deferred.t)
  -> [> `Deprecated_use_Scheduler_block_on_async]

(** [syscall f] runs f, which should be a single system call, and returns the result,
    handling the restarting of interrupted system calls.  To avoid race conditions, the
    [f] supplied to [syscall] should just make a system call.  That way, everything else
    is done holding the Async lock. *)
val syscall : (unit -> 'a) -> ('a, exn) Result.t Deferred.t
val syscall_exn : (unit -> 'a) -> 'a Deferred.t

(** [am_in_async ()] returns true if the currently running thread is holding the async
    lock. *)
val am_in_async : unit -> bool
