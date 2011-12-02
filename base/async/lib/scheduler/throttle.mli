(** Throttles for simultaneous computations.

    A throttle schedules asynchronous computations so that at any given point in time no
    more than [max_concurrent_jobs] jobs are running.  A job [f] is considered to be
    running from the time [f ()] is executed until the deferred returned by [f ()] becomes
    determined, or [f ()] throws an exception.

    A throttle becomes "dead" if one of its jobs throws an exception, and the throttle is
    not set to continue on error. *)


open Core.Std
open Import

type t

val invariant : t -> unit

(** [create ~continue_on_error ~max_concurrent_jobs] returns a throttle that will runs up
    to [max_concurrent_jobs] concurrently.

    If some job raises an exception, then the throttle will stop, unless
    [continue_on_error] is true. *)
val create : continue_on_error:bool -> max_concurrent_jobs:int -> t

(** [enqueue t ~monitor job] schedules [job] to be run as soon as possible.  Jobs are
    guaranteed to be started in the order they are scheduled.

    [enqueue] raises an exception if the throttle is dead. *)
val enqueue : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t


(** [is_empty t] is true iff there are no running jobs and no enqueued jobs on the
    throttle
*)
val is_empty : t -> bool

module Slot : sig
  type 'a t

  (* [run slot work] runs [work] in [slot], which was reserved by [reserve_slot].  It
     is an error to call [run] more than once on the same slot.

     Any errors raised by [work] will be handled by the throttle that reserved the slot
     and raised to the monitor in effect when [reserve_slot] was called. *)
  val run : 'a t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

  val release : unit t -> unit
end

(** [reserve_slot t] returns a deferred that will become determined when there is a free
    slot on the throttle. The slot can then be used to immediately run a job in the
    throttle.

    The use case for this function is a situation where you have many jobs which you would
    like to eventually run, however there are so many that you want to throttle not just
    how many are running at once, but also the actual creation of the jobs.

    Then you can do something like

    let rec loop () =
      Throttle.reserve_slot throttle >>> fun slot ->
      match next_job () with
      | Some f -> whenever (Throttle.Slot.run slot f); loop ()
      | None -> ()
    in
    loop ()

    where [next_job] is the function which produces jobs.
*)
val reserve_slot : t -> _ Slot.t Deferred.t

(* [prior_jobs_done t] becomes determined when all of the jobs that were enqueued in [t]
   prior to its call have completed. *)
val prior_jobs_done : t -> unit Deferred.t

(** A sequencer is a throttle that is simultaneously:

    (1) specialized to only allow one job at a time and to not continue on error
    (2) generalized to carry its own state, and enforce mutually exclusive access to that
    state by the enqueued jobs *)
module Sequencer : sig
  type 'a t

  (** create a new monitor with the specified initial state *)
  val create : ?continue_on_error:bool -> 'a -> 'a t

  (** schedule a state-accessing operation *)
  val enqueue : 'a t -> ('a -> 'b Deferred.t) -> 'b Deferred.t
end
