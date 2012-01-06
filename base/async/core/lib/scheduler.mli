open Core.Std
open Basic

val invariant : unit -> unit
val current_execution_context  : unit -> Execution_context.t
val set_block_group            : Block_group.t -> unit
val set_main_execution_context : Execution_context.t -> unit
val with_execution_context     : Execution_context.t -> f:(unit -> 'a) -> 'a
val add_job                    : Execution_context.t -> (unit -> unit) -> unit
val main_execution_context     : unit -> Execution_context.t
val cycle_start : unit -> Time.t
val start_cycle : now:Time.t -> unit
val finish_cycle : now:Time.t -> unit
val jobs_left : unit -> bool
val next_upcoming_event : unit -> Time.t option
val uncaught_exception : unit -> exn option
val num_pending_jobs : unit -> int
val num_jobs_run : unit -> int
val cycle_times : unit -> Time.Span.t Stream.t
val cycle_num_jobs : unit -> int Stream.t
val cycle_count : unit -> int
val set_max_num_jobs_per_priority_per_cycle : int -> unit

val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

type 'a with_options =
     ?block_group:Block_group.t
  -> ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a
val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val within    : ((unit -> unit         ) -> unit         ) with_options
val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val schedule  : ((unit -> unit         ) -> unit         ) with_options
