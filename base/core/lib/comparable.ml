module type Infix = sig
  type t
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type S_common = sig
  include Infix
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val ascending : t -> t -> int
  val descending : t -> t -> int
  val min : t -> t -> t
  val max : t -> t -> t
end

module type S = sig
  include S_common

  module Map : Core_map.S with type key = t
  module Set : Core_set.S with type elt = t
end

module type S_binable = sig
  include S_common

  module Map : Core_map.S_binable with type key = t
  module Set : Core_set.S_binable with type elt = t
end

module Poly (T : sig
  type t
  include Sexpable.S with type t := t
end) : S with type t := T.t = struct
  type t = T.t
  include Pervasives                    (* for Infix *)
  let ascending = compare
  let descending x y = compare y x
  let equal = (=)
  module C = struct
    include T
    let compare = compare
  end
  module Map = Core_map.Make (C)
  module Set = Core_set.Make (C)
end

module Make_common (T : sig
  type t
  include Sexpable.S with type t := t
  val compare : t -> t -> int
end) = struct
  type t = T.t

  let compare = T.compare
  let ascending = compare
  let descending t t' = compare t' t

  module Infix = struct
    let (>) a b = compare a b > 0
    let (<) a b = compare a b < 0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=) a b = compare a b = 0
    let (<>) a b = compare a b <> 0
  end
  include Infix

  let equal = (=)
  let min t t' = if t <= t' then t else t'
  let max t t' = if t >= t' then t else t'
end

module Make (T : sig
  type t
  include Sexpable.S with type t := t
  val compare : t -> t -> int
end) : S with type t := T.t = struct
  include Make_common (T)

  module Map = Core_map.Make (T)
  module Set = Core_set.Make (T)
end

module Make_binable (T : sig
  type t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
  val compare : t -> t -> int
end) : S_binable with type t := T.t = struct
  include Make_common (T)

  module Map = Core_map.Make_binable (T)
  module Set = Core_set.Make_binable (T)
end

(** Inherit comparability from a component. *)
module Inherit (C : S)
  (T : sig
    type t
    include Sexpable.S with type t := t
    val component : t -> C.t
  end)
  : S with type t = T.t = struct

    type t = T.t
    (* We write [binary] in this way for performance reasons.  It is always
     * applied to one argument and builds a two-argument closure.
     *)
    let binary f = (); fun t t' -> f (T.component t) (T.component t')
    let compare = binary C.compare
    let (>=) = binary C.(>=)
    let (<=) = binary C.(<=)
    let (=) = binary C.(=)
    let equal = (=)
    let (>) = binary C.(>)
    let (<) = binary C.(<)
    let (<>) = binary C.(<>)
    let ascending = binary C.ascending
    let descending = binary C.descending
    let min t t' = if t <= t' then t else t'
    let max t t' = if t >= t' then t else t'

    module M = struct
      include T
      let compare = compare
    end

    module Map = Core_map.Make (M)
    module Set = Core_set.Make (M)
end

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps -> let res = cmp x y in if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;
