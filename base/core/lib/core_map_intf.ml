
open Sexplib

module type Key = sig
  type t
  include Sexpable.S with type t := t

  val compare : t -> t -> int
end

module type Types = sig
  type 'k key
  type ('k, +'v) t
end

(* Gen is a signature functor that generalizes the monomorpic maps and
   polymorphic maps. *)
module Gen (T : Types) = struct
  open T

  module type S = sig
    (** the empty map *)
    val empty: (_, _) t

    (** map with one key, data pair *)
    val singleton: 'k key -> 'v -> ('k, 'v) t

    (** Test whether a map is empty or not. *)
    val is_empty: (_, _) t -> bool

    (** [length map] @return number of elements in [map]. *)
    val length : (_, _) t -> int

    (** returns a new map with the specified new binding;
        if the key was already bound, its previous binding disappears. *)
    val add: ('k, 'v) t -> key:'k key -> data:'v -> ('k, 'v) t

    (** if key is not present then add a singleton list, otherwise, cons data
        on the head of the existing list. *)
    val add_multi: ('k, 'v list) t -> key:'k key -> data:'v -> ('k, 'v list) t

    (** [change map key f] updates the given map by changing the value stored
        under [key] according to [f].  Thus, for example, one might write:

        {[change m k (function None -> Some 0 | Some x -> Some (x + 1))]}

        to produce a new map where the integer stored under key [k] is
        incremented by one (treating an unknown key as zero) *)
    val change : ('k, 'v) t -> 'k key -> ('v option -> 'v option) -> ('k,'v) t


    (** returns the value bound to the given key, raising [Not_found] if none
        such exists *)
    val find_exn: ('k, 'v) t -> 'k key -> 'v

    val find: ('k, 'v) t -> 'k key -> 'v option

    (** returns a new map with any binding for the key in question removed *)
    val remove: ('k, 'v) t -> 'k key -> ('k, 'v) t

    (** [mem key map] tests whether [map] contains a binding for [key] *)
    val mem: ('k, _) t -> 'k key -> bool

    (** iterator for map *)
    val iter: ('k, 'v) t -> f:(key:'k key -> data:'v -> unit) -> unit

    (** returns new map with bound values replaced by f applied to the bound values *)
    val map: ('k, 'v1) t -> f:('v1 -> 'v2) -> ('k, 'v2) t

    (** like [map], but function takes both key and data as arguments *)
    val mapi: ('k, 'v1) t -> f:(key:'k key -> data:'v1 -> 'v2) -> ('k, 'v2) t

    (** folds over keys and data in map *)
    val fold:
      ('k, 'v) t -> f:(key:'k key -> data:'v -> 'a -> 'a) -> init:'a -> 'a

    (** folds over keys and data in map in reverse order *)
    val fold_right:
      ('k, 'v) t -> f:(key:'k key -> data:'v -> 'a -> 'a) -> init:'a -> 'a

    (** filter for map *)
    val filter:('k, 'v) t -> f:(key:'k key -> data:'v -> bool) -> ('k, 'v) t

    (** returns new map with bound values filtered by f applied to the bound
        values *)
    val filter_map: ('k, 'v1) t -> f:('v1 -> 'v2 option) -> ('k, 'v2) t

    (** like [filter_map], but function takes both key and data as arguments*)
    val filter_mapi:
      ('k, 'v1) t -> f:(key:'k key -> data:'v1 -> 'v2 option) -> ('k, 'v2) t

    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)
    val compare: ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int

    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
        equal, that is, contain equal keys and associate them with
        equal data.  [cmp] is the equality predicate used to compare
        the data associated with the keys. *)
    val equal: ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool

    (** returns list of keys in map *)
    val keys: ('k, _) t -> 'k key list

    (** equivalent to [mem] *)
    val has_key: ('k, _) t -> 'k key -> bool

    (** returns list of data in map *)
    val data: (_, 'v) t -> 'v list

    (** creates map from association list with unique keys *)
    val of_alist:
      ('k key * 'v) list -> [ `Ok of ('k, 'v) t | `Duplicate_key of 'k key ]

    (** creates map from association list with unique keys.  Raises Failure if
        duplicate 'a keys are found. *)
    val of_alist_exn: ('k key * 'v) list -> ('k, 'v) t

    (** creates map from association list with possibly repeated keys. *)
    val of_alist_multi: ('k key * 'v) list -> ('k, 'v list) t

    (** creates association list from map.  No guarantee about order. *)
    val to_alist: ('k, 'v) t -> ('k key * 'v) list

    (** {6 Additional operations on maps} *)

    (** combines an association list into a map, folding together the bound
        values (for experts only) *)
    val of_alist_fold:
      ('k key * 'v1) list -> init:'v2 -> f:('v2 -> 'v1 -> 'v2) -> ('k, 'v2) t

    (** merges two maps *)
    val merge:
      ('k, 'v1) t
      -> ('k, 'v2) t
      -> f:(key:'k key
          -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
          -> 'v3 option)
      -> ('k, 'v3) t

    (** [min_elt map] @return Some [(key, data)] pair corresponding to the
        minimum key in [map], None if empty. *)
    val min_elt : ('k, 'v) t -> ('k key * 'v) option

    (** [min_elt_exn map] @return the [(key, data)] pair corresponding to the minimum key
        in [map], and raises if [map] is empty. *)
    val min_elt_exn : ('k, 'v) t -> 'k key * 'v

    (** [max_elt map] @return Some [(key, data)] pair corresponding to the
        maximum key in [map], and None if [map] is empty. *)
    val max_elt : ('k, 'v) t -> ('k key * 'v) option

    (** [max_elt_exn map] @return the [(key, data)] pair corresponding to the
        maximum key in [map], raises an exception if [map] is empty. *)
    val max_elt_exn : ('k, 'v) t -> 'k key * 'v

    (** same semantics as similar functions in List *)
    val for_all : ('k, 'v) t -> f:('v -> bool) -> bool
    val exists  : ('k, 'v) t -> f:('v -> bool) -> bool

    (** [fold_range_inclusive t ~min ~max ~init ~f]
        folds f (with initial value ~init) over all keys (and their associated values)
        that are in the range [min, max] (inclusive)
    *)
    val fold_range_inclusive :
      ('k, 'v) t -> min:'k key -> max:'k key -> init:'a
      -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

    (** [range_to_alist t ~min ~max] returns an associative list of the elements whose
        keys lie in [min, max] (inclusive), with the smallest key being at the head of the
        list
    *)
    val range_to_alist : ('k, 'v) t -> min:'k key -> max:'k key -> ('k key * 'v) list

    (** [prev_key t k] returns the largest (key, value) pair in t with key less than k *)
    val prev_key : ('k, 'v) t -> 'k key -> ('k key * 'v) option
    (** [next_key t k] returns the smallest (key, value) pair in t with key greater than k *)
    val next_key : ('k, 'v) t -> 'k key -> ('k key * 'v) option
    (** [rank t k] if k is in t, returns the number of keys strictly less than k in t,
        otherwise None *)
    val rank : ('k, 'v) t -> 'k key -> int option
  end
end

module type S = sig
  type key
  type +'a t
  module T : Types with type 'a key = key with type ('a, +'b) t = 'b t

  include Sexpable.S1 with type +'a t := 'a t

  val empty: _ t
  val singleton: key -> 'a -> 'a t
  val is_empty: _ t -> bool
  val length : _ t -> int
  val add: 'a t -> key:key -> data:'a -> 'a t
  val add_multi: 'a list t -> key:key -> data:'a -> 'a list t
  val change : 'a t -> key -> ('a option -> 'a option) -> 'a t
  val find_exn: 'a t -> key -> 'a
  val find: 'a t -> key -> 'a option
  val remove: 'a t -> key -> 'a t
  val mem: 'a t -> key -> bool
  val iter: 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val map: 'a t -> f:('a -> 'b) -> 'b t
  val mapi: 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t
  val fold: 'a t -> f:(key:key -> data:'a -> 'b -> 'b) -> init:'b -> 'b
  val fold_right: 'a t -> f:(key:key -> data:'a -> 'b -> 'b) -> init:'b -> 'b

  val filter: 'a t -> f:(key:key -> data:'a -> bool) -> 'a t
  val filter_map: 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi: 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val keys: 'a t -> key list
  val has_key: 'a t -> key -> bool
  val data: 'a t -> 'a list
  val of_alist: (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_alist_exn: (key * 'a) list -> 'a t
  val of_alist_multi: (key * 'a) list -> 'a list t
  val to_alist: 'a t -> (key * 'a) list
  val of_alist_fold: (key * 'b) list -> init:'a -> f:('a -> 'b -> 'a) -> 'a t
  val merge:
    'data1 t
    -> 'data2 t
    -> f:(key:key
          -> [ `Left of 'data1 | `Right of 'data2 | `Both of 'data1 * 'data2 ]
          -> 'data3 option)
    -> 'data3 t
  val min_elt : 'data t -> (key * 'data) option
  val min_elt_exn : 'data t -> key * 'data
  val max_elt : 'data t -> (key * 'data) option
  val max_elt_exn : 'data t -> key * 'data
  val for_all : 'data t -> f:('data -> bool) -> bool
  val exists  : 'data t -> f:('data -> bool) -> bool
  val fold_range_inclusive :
    'data t -> min:key -> max:key -> init:'a
    -> f:(key:key -> data:'data -> 'a -> 'a) -> 'a
  val range_to_alist : 'data t -> min:key -> max:key -> (key * 'data) list
  val prev_key : 'data t -> key -> (key * 'data) option
  val next_key : 'data t -> key -> (key * 'data) option
  val rank : 'a t -> key -> int option
end

module type S2 = sig
  type ('k, +'v) t
  module T : Types with type 'a key = 'a with type ('a, +'b) t = ('a, 'b) t

  include Binable.S2 with type ('a, +'b) t := ('a, 'b) t
  include Sexpable.S2 with type ('a, +'b) t := ('a, 'b) t

  val empty: ('k, 'v) t
  val singleton: 'k -> 'v -> ('k, 'v) t
  val is_empty: ('k, 'v) t -> bool
  val length : (_, _) t -> int
  val add: ('k, 'v) t -> key:'k -> data:'v -> ('k, 'v) t
  val add_multi: ('k, 'v list) t -> key:'k -> data:'v -> ('k, 'v list) t
  val change : ('k, 'v) t -> 'k -> ('v option -> 'v option) -> ('k,'v) t
  val find_exn: ('k, 'v) t -> 'k -> 'v
  val find: ('k, 'v) t -> 'k -> 'v option
  val remove: ('k, 'v) t -> 'k -> ('k, 'v) t
  val mem: ('k, 'v) t -> 'k -> bool
  val iter: ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit
  val map: ('k, 'v) t -> f:('v -> 'b) -> ('k, 'b) t
  val mapi: ('k, 'v) t -> f:(key:'k -> data:'v -> 'b) -> ('k, 'b) t
  val fold: ('k, 'v) t -> f:(key:'k -> data:'v -> 'b -> 'b) -> init:'b -> 'b
  val fold_right: ('k, 'v) t -> f:(key:'k -> data:'v -> 'b -> 'b) -> init:'b -> 'b
  val filter: ('k, 'v) t -> f:(key:'k -> data:'v -> bool) -> ('k, 'v) t
  val filter_map: ('k, 'v) t -> f:('v -> 'b option) -> ('k, 'b) t
  val filter_mapi: ('k, 'v) t -> f:(key:'k -> data:'v -> 'b option) -> ('k, 'b) t
  val compare: ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
  val equal: ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool
  val keys: ('k, 'v) t -> 'k list
  val has_key: ('k, 'v) t -> 'k -> bool
  val data: ('k, 'v) t -> 'v list
  val of_alist: ('k * 'v) list -> [ `Ok of ('k, 'v) t | `Duplicate_key of 'k ]
  val of_alist_exn: ('k * 'v) list -> ('k, 'v) t
  val of_alist_multi: ('k * 'v) list -> ('k, 'v list) t
  val to_alist: ('k, 'v) t -> ('k * 'v) list
  val of_alist_fold: ('k * 'v1) list -> init:'v2 -> f:('v2 -> 'v1 -> 'v2) -> ('k, 'v2) t
  val merge:
    ('k, 'data1) t
    -> ('k, 'data2) t
    -> f:(key:'k
          -> [ `Left of 'data1 | `Right of 'data2 | `Both of 'data1 * 'data2 ]
          -> 'data3 option)
    -> ('k, 'data3) t
  val min_elt : ('k, 'data) t -> ('k * 'data) option
  val min_elt_exn : ('k, 'data) t -> 'k * 'data
  val max_elt : ('k, 'data) t -> ('k * 'data) option
  val max_elt_exn : ('k, 'data) t -> 'k * 'data
  val for_all : ('k, 'data) t -> f:('data -> bool) -> bool
  val exists  : ('k, 'data) t -> f:('data -> bool) -> bool
  val fold_range_inclusive :
    ('k, 'data) t -> min:'k -> max:'k -> init:'a
    -> f:(key:'k -> data:'data -> 'a -> 'a) -> 'a
  val range_to_alist : ('k, 'data) t -> min:'k -> max:'k -> ('k * 'data) list

  val next_key : ('k, 'data) t -> 'k -> ('k * 'data) option
  val prev_key : ('k, 'data) t -> 'k -> ('k * 'data) option
  val rank : ('k, 'data) t -> 'k -> int option
end

module type Gen = sig
  module T : Types
  include Gen(T).S
end

(* Check that S and S2 are instances of Gen *)
module Check_S (M : S) = (M : Gen(M.T).S)
module Check_S2 (M : S2) = (M : Gen(M.T).S)
