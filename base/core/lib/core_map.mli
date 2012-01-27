(** Polymorphic map module.  If you open Core.Std, this is your Map module. *)

(** {6 Polymorphic versions of standard Map operations} *)

open Core_map_intf

type ('key, +'value, 'comparator) t

include Creators
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  with type 'a key = 'a
  with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_with_comparator_required

include Accessors
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  with type 'a key := 'a key

module Poly : sig
  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type ('a, 'b) t = ('a, 'b, Comparator.Poly.comparator) map with bin_io, sexp
  type ('a, 'b, 'c) t_ = ('a, 'b) t

  include Creators
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type 'a key = 'a
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_without_comparator

  (* [empty] has the same spec in [Creators], but adding it here prevents a type-checker
     issue with nongeneralizable type variables. *)
  val empty : ('a, 'b) t

  include Accessors
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type 'a key := 'a key
end
  with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

module type Key = Key
module type S         = S         with type ('a, 'b, 'c) map := ('a, 'b, 'c) t
module type S_binable = S_binable with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

module Make                  (Key : Comparator.Pre) : S with type Key.t = Key.t
module Make_using_comparator (Key : Comparator.S  ) : S with type Key.t = Key.t

module Make_binable (Key : Comparator.Pre_binable)
  : S_binable with type Key.t = Key.t

module Make_binable_using_comparator (Key : Comparator.S_binable)
  : S_binable with type Key.t = Key.t
