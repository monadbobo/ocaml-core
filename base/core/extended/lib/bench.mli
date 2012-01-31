open Core.Std



module Test : sig
  type t
  val create : ?name:string -> ?size:int -> (unit -> unit) -> t
  val name : t -> string option
  val size : t -> int option
end

module Result : sig
  module Stat : sig
    type t = {
      run_time : int;
      gc_time : int;
      sample_size : int;
      compactions : int;
      allocated : int;
    }

    val empty : t
  end

  type t = string option * int option * Stat.t array

  val mean : Stat.t array -> Stat.t
  val min : Stat.t array -> Stat.t
  val max : Stat.t array -> Stat.t

  val sdev : Stat.t array -> int
  val compactions_occurred : Stat.t array -> bool
  val sample_size : Stat.t array -> int
end

(* In these functions, gc_prefs can be used to change the GC settings during testing and
   no_compactions can be used to disable GC compactions.  Both of these are reset after
   the benchmarking is done and no_compactions takes precedence over gc_prefs.  Low
   verbosity will print only the output of the benchmarking, mid will additionally print
   time estimates and a status line, and high will additionally print information at each
   step (default low). fast can be set to true to perform 1/100th as many tests.
   ?clock controls time measurement method: `Wall will include waiting on I/O or when
   process is suspended/descheduled; `Cpu will only count time spent on computations.
 *)

type ('a, 'b) benchmark_function =
  ?verbosity:[ `High | `Mid | `Low ]
  -> ?gc_prefs:Gc.Control.t
  -> ?no_compactions:bool
  -> ?fast:bool
  -> ?clock:[`Wall | `Cpu]
  -> 'a -> 'b

type 'a print_function =
  ?time_format:[`Ns | `Ms | `Us | `S | `Auto]
  -> 'a

val bench : (Test.t list, unit) benchmark_function print_function

(* Returns a list documenting the runtimes rather than printing to stdout. These can be
   fed to print for results identical to calling bench. *)
val bench_raw : (Test.t list, Result.t list) benchmark_function

val print : (Result.t list -> unit) print_function

module Bundle : sig
  type 'a t

  val create : 'a -> 'a t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val bench : 'a t -> unit
end
