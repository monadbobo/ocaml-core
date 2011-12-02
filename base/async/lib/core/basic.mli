(** Basic contains the type declarations and basic functions for most of the Async
    types: deferred, stream, ivar, tail, monitor, scheduler. *)

open Core.Std

type 'a ivar
type 'a handler

module Unregister : sig
  type t

  (** no-operation unregister *)
  val noop : t

  (** [create f] creates an unregister of the given function [f]. *)
  val create : (unit -> unit) -> t

  (** [unregister t] executes the unregistration function in [t]. *)
  val unregister : t -> unit
end

module Deferred : sig
  type +'a t with sexp_of

  val bind          : 'a t -> ('a -> 'b t) -> 'b t
  val create        : ('a ivar -> unit) -> 'a t
  val is_determined : _ t -> bool
  val peek          : 'a t -> 'a option
  val return        : 'a -> 'a t
  val upon          : 'a t -> ('a -> unit) -> unit
  val upon'         : 'a t -> ('a -> unit) -> Unregister.t
  val install_removable_handler : 'a t -> 'a handler -> Unregister.t

  val debug_space_leaks   : int option ref
  val debug_trace_connect : bool       ref
end

module Ivar : sig
  type 'a t = 'a ivar with bin_io, sexp_of

  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  val equal : 'a t -> 'a t -> bool
  val create : unit -> 'a t
  val fill : 'a t -> 'a -> unit
  val fill_if_empty : 'a t -> 'a -> unit
  val is_empty : 'a t -> bool
  val is_full : 'a t -> bool
  val read : 'a t -> 'a Deferred.t
end

module Stream : sig
  type 'a t = 'a next Deferred.t
  and 'a next = Nil | Cons of 'a * 'a t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val next : 'a t -> 'a next Deferred.t
  val iter : 'a t -> f:('a -> unit) -> unit
end

module Tail : sig
  type 'a t =
    { (* [next] points at the tail of the stream *)
      mutable next: 'a Stream.next Ivar.t;
    }

  val create : unit -> _ t
end

module Monitor : sig
  type t =
    { name_opt : string option;
      id : int;
      parent : t option;
      errors : exn Tail.t;
      mutable has_seen_error : bool;
      mutable someone_is_listening : bool;
    }
  with fields
end

module Execution_context : sig
  type t =
    { block_group : Block_group.t;
      monitor : Monitor.t;
      priority : Jobs.Priority.t;
    }
  with fields
end

module Handler : sig
  type 'a t = 'a handler

  val create : ('a -> unit) -> 'a t
  val prepend : 'a t -> f:('b -> 'a) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val schedule : 'a t -> 'a -> unit
end

module Clock_event : sig
  type t =
    { mutable state : state;
    }
  and state =
  | Uninitialized
  | Aborted
  | Happened
  | Waiting of waiting
  and waiting =
    { event : t Events.Event.t;
      ready : [ `Happened | `Aborted ] Ivar.t;
    }
  with sexp_of
end

module Scheduler : sig
  type t =
    { jobs : Jobs.t sexp_opaque;
      mutable current_execution_context : Execution_context.t sexp_opaque;
      mutable main_execution_context : Execution_context.t sexp_opaque;
      mutable max_num_jobs_per_priority_per_cycle : int;
      mutable uncaught_exception : exn option;
      mutable cycle_count : int;
      mutable cycle_start : Time.t;
      mutable jobs_left : bool;
      cycle_times : Time.Span.t Tail.t;
      events : Clock_event.t Events.t;
    }

  val t : t
  val add_job                    : Execution_context.t -> (unit -> unit) -> unit
  val current_execution_context  : unit -> Execution_context.t
  val set_execution_context      : Execution_context.t -> unit
  val invariant : unit -> unit
end
