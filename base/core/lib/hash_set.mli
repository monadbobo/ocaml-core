(* A mutable set of elements *)


type 'a t

val copy : 'a t -> 'a t                 (* preserves the equality function *)
val add : 'a t -> 'a -> unit
val strict_add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val strict_remove : 'a t -> 'a -> unit
val clear : 'a t -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val iter : 'a t -> f:('a -> unit) -> unit
val length : 'a t -> int
val mem : 'a t -> 'a -> bool
val is_empty : 'a t -> bool
val to_list : 'a t -> 'a list
val equal : 'a t -> 'a t -> bool
val filter : 'a t -> f:('a -> bool) -> 'a t
val diff : 'a t -> 'a t -> 'a t
val of_hashtbl_keys : ('a, _) Core_hashtbl.t -> 'a t
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val filter_inplace : 'a t -> f:('a -> bool) -> unit

type 'a hash_set = 'a t

(* A hash set that uses polymorphic comparison *)
module Poly : sig
  type 'a t = 'a hash_set
  include Sexpable.S1 with type 'a t := 'a t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> 'a t
  val of_list : 'a list -> 'a t
end

module Make (H : Core_hashtbl.Key) : sig
  type elem = H.t
  type t = elem hash_set
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : H.t list -> t
  include Sexpable.S with type t := t
end

module Make_binable (H : sig
  include Core_hashtbl.Key
  include Binable.S with type t := t
end) : sig
  type elem = H.t
  type t = elem hash_set
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : H.t list -> t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
end
