
open Sexplib


module type Elt = sig
  type t
  include Sexpable.S with type t := t

  val compare : t -> t -> int
end

module type Types = sig
  type +'e elt
  type +'e t
end

module Gen(T : Types) = struct
  open T
  module type S = sig
    val empty: 'e t
    val is_empty: 'e t -> bool
    val mem: 'e t -> 'e elt -> bool
    val add: 'e t -> 'e elt -> 'e t
    val singleton: 'e elt -> 'e t
    val remove: 'e t -> 'e elt -> 'e t
    val union: 'e t -> 'e t -> 'e t
    val union_list : 'e t list -> 'e t
    val inter: 'e t -> 'e t -> 'e t
    val diff: 'e t -> 'e t -> 'e t
    val compare: 'e t -> 'e t -> int
    val equal: 'e t -> 'e t -> bool
    val subset: 'e t -> 'e t -> bool
    val iter: 'e t -> f:('e elt -> unit) -> unit
    val fold: 'e t -> init:'a -> f:('e elt -> 'a -> 'a) -> 'a
    val fold_until: 'e t -> init:'a -> f:('e elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> 'a
    val for_all: 'e t -> f:('e elt -> bool) -> bool
    val exists: 'e t -> f:('e elt -> bool) -> bool
    val filter: 'e t -> f:('e elt -> bool) -> 'e t
    val partition: 'e t -> f:('e elt -> bool) -> 'e t * 'e t
    val length : _ t -> int
    val elements: 'e t -> 'e elt list
    val min_elt: 'e t -> 'e elt option
    val min_elt_exn: 'e t -> 'e elt
    val max_elt: 'e t -> 'e elt option
    val max_elt_exn: 'e t -> 'e elt
    val choose: 'e t -> 'e elt option
    val choose_exn: 'e t -> 'e elt
    val of_list: 'e elt list -> 'e t
    val to_list: 'e t -> 'e elt list
    val of_array: 'e elt array -> 'e t
    val to_array: 'e t -> 'e elt array
    val split: 'e elt -> 'e t -> 'e t * bool * 'e t
    val group_by: 'e t -> equiv:('e elt -> 'e elt -> bool) -> 'e t list
    val find: 'e t -> f:('e elt -> bool) -> 'e elt option
    val find_exn: 'e t -> f:('e elt -> bool) -> 'e elt
    val find_map: 'e t -> f:('e elt -> 'a option) -> 'a option
    (* Returns the ith smallest element in the set in O(log n) time.  The smallest
       element is element 0. *)
    val find_index : 'e t -> int -> 'e elt option
    val remove_index : 'e t -> int -> 'e t
    (* stable_dedup is here rather than in the List module because the implementation
       relies crucially on sets, and because doing so allows one to avoid uses
       of polymorphic comparison by instantiating the functor at a different
       implementation of Comparable and using the resulting [stable_dedup_list]. *)
    val stable_dedup_list : 'e elt list -> 'e elt list
  end
end

module type S = sig
  type elt
  type t
  module T : Types with type 'a elt = elt with type 'e t = t
  include Sexpable.S with type t := t
  val empty: t
  val is_empty: t -> bool
  val mem: t -> elt -> bool
  val add: t -> elt -> t
  val singleton: elt -> t
  val remove: t -> elt -> t
  val union: t -> t -> t
  val union_list : t list -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: t -> f:(elt -> unit) -> unit
  val fold: t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a
  val fold_until: t -> init:'a -> f:(elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> 'a
  val for_all  : t -> f:(elt -> bool) -> bool
  val exists   : t -> f:(elt -> bool) -> bool
  val filter   : t -> f:(elt -> bool) -> t
  val partition: t -> f:(elt -> bool) -> t * t
  val length     : t -> int
  val elements   : t -> elt list
  val min_elt    : t -> elt option
  val min_elt_exn: t -> elt
  val max_elt    : t -> elt option
  val max_elt_exn: t -> elt
  val choose     : t -> elt option
  val choose_exn : t -> elt
  val of_list: elt list -> t
  val to_list: t -> elt list
  val of_array: elt array -> t
  val to_array: t -> elt array
  val split: elt -> t -> t * bool * t
  val group_by: t -> equiv:(elt -> elt -> bool) -> t list
  val find: t -> f:(elt -> bool) -> elt option
  val find_exn: t -> f:(elt -> bool) -> elt
  val find_map: t -> f:(elt -> 'a option) -> 'a option
  val find_index : t -> int -> elt option
  val remove_index : t -> int -> t
  val setify : elt list -> elt list
  val stable_dedup_list : elt list -> elt list
end

module type S1 = sig
  type +'elt t
  module T : Types with type 'a elt = 'a with type 'e t = 'e t
  include Sexpable.S1 with type 'elt t := 'elt t
  include Binable.S1 with type 'elt t := 'elt t
  val empty: 'elt t
  val is_empty: 'elt t -> bool
  val mem: 'elt t -> 'elt -> bool
  val add: 'elt t -> 'elt -> 'elt t
  val singleton: 'elt -> 'elt t
  val remove: 'elt t -> 'elt -> 'elt t
  val union: 'elt t -> 'elt t -> 'elt t
  val union_list : 'elt t list -> 'elt t
  val inter: 'elt t -> 'elt t -> 'elt t
  val diff: 'elt t -> 'elt t -> 'elt t
  val compare: 'elt t -> 'elt t -> int
  val equal: 'elt t -> 'elt t -> bool
  val subset: 'elt t -> 'elt t -> bool
  val iter: 'elt t -> f:('elt -> unit) -> unit
  val fold: 'elt t -> init:'a -> f:('elt -> 'a -> 'a) -> 'a
  val fold_until: 'elt t -> init:'a -> f:('elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> 'a
  val for_all: 'elt t -> f:('elt -> bool) -> bool
  val exists : 'elt t -> f:('elt -> bool) -> bool
  val filter : 'elt t -> f:('elt -> bool) -> 'elt t
  val filter_map: 'elt t -> f:('elt -> 'a option) -> 'a            t
  val partition : 'elt t -> f:('elt -> bool     ) -> 'elt t * 'elt t
  val length : _ t -> int
  val elements: 'elt t -> 'elt list
  val min_elt: 'elt t -> 'elt option
  val min_elt_exn: 'elt t -> 'elt
  val max_elt: 'elt t -> 'elt option
  val max_elt_exn: 'elt t -> 'elt
  val choose: 'elt t -> 'elt option
  val choose_exn: 'elt t -> 'elt
  val of_list: 'elt list -> 'elt t
  val to_list: 'elt t -> 'elt list
  val of_array: 'elt array -> 'elt t
  val to_array: 'elt t -> 'elt array
  val map: f:('a -> 'b) -> 'a t -> 'b t
  val split: 'elt -> 'elt t -> 'elt t * bool * 'elt t
  val group_by: 'elt t -> equiv:('elt -> 'elt -> bool) -> 'elt t list
  val find: 'elt t -> f:('elt -> bool) -> 'elt option
  val find_exn: 'elt t -> f:('elt -> bool) -> 'elt
  val find_map: 'e t -> f:('e -> 'a option) -> 'a option
  val find_index : 'elt t -> int -> 'elt option
  val remove_index : 'elt t -> int -> 'elt t
  val setify : 'elt list -> 'elt list
  val stable_dedup_list : 'elt list -> 'elt list
end

module type Gen = sig
  module T : Types
  include Gen(T).S
end

module Check_S (M : S)   = (M : Gen(M.T).S)
module Check_S1 (M : S1) = (M : Gen(M.T).S)
