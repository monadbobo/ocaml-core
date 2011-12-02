include Gc

(** [finalise] is rebound to avoid accidental use in Async programs. *)
let finalise
    (`Should_not_use_finalise_in_async as x)
    `Should_not_use_finalise_in_async = x
