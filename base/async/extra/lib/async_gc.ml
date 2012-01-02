open Core.Std

include Gc

(** [finalize f x] is like [Gc.finalise f x], except that the finalizer is guaranteed to
    run as an Async job (i.e. without interrupting other Async jobs).  Unprotected use of
    [Gc.finalise] in Async programs is wrong, because finalizers can run at any time and
    in any thread. *)
let finalize : ('a -> unit) -> 'a -> unit = Async_scheduler.Raw_scheduler.finalize

(** [finalise] is rebound to avoid accidental use in Async programs. *)
let finalise (`In_async__use_finalize_not_finalise as x) _ = x
