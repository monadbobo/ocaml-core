
(* These are just the creation functions for non-polymorphic Hash_sets.  Most of the
   functions live directly in Hash_set.  E.g.

   let my_set = Int.Hash_set.create in
   Hash_set.add my_set 3
*)

module type S = sig
  type elem
  type t = elem Hash_set.t
  include Sexpable.S with type t := t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : elem list -> t
end

module type S_binable = sig
  type elem
  type t = elem Hash_set.t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : elem list -> t
end
