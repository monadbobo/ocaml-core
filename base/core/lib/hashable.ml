open Core_hashtbl_intf

module Hashtbl = Core_hashtbl

module Table_sig (Key : T0) = Monomorphic (Hashtbl) (Key)

module type S = sig
  type t
  module Hashable : T0 with type t = t
  val hash : t -> int
  module Table : Table_sig (Hashable).S
  module Hash_set : Hash_set_intf.S with type elem = t
  module Hash_queue : Hash_queue.S with type Key.t = t
  module Hash_heap : Hash_heap.S with type Key.t = t
end

module Make (T : Hashtbl.Key) : S with type t := T.t = struct
  include T
  module Hashable = T
  module Table = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)
end

module type S_binable = sig
  type t
  module Hashable : T0 with type t = t
  val hash : t -> int
  module Table : sig
    include Core_hashtbl_intf.Monomorphic (Hashtbl) (Hashable).S
    include Binable.S1 with type 'a t := 'a t
  end
  module Hash_set : Hash_set_intf.S_binable with type elem = t
  module Hash_queue : Hash_queue.S with type Key.t = t
  module Hash_heap : Hash_heap.S with type Key.t = t
end

module Make_binable (T : sig
  include Hashtbl.Key
  include Binable.S with type t := t
end) : S_binable with type t := T.t = struct
  module Table = Hashtbl.Make_binable (T)
  module Hashable = T
  module Hash_set = Hash_set.Make_binable (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)

  include T
end
