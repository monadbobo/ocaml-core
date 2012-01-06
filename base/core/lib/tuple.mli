module T2 : sig
  type ('a, 'b) t = 'a * 'b

  include Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t

  val create : 'a -> 'b -> ('a, 'b) t
  val curry :  (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
  val compare : cmp1: ('a -> 'a -> int) -> cmp2:('b -> 'b -> int)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> int

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"
  val map1 : f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map2 : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val swap : ('a, 'b) t -> ('b, 'a) t
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  include Sexpable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val create : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  val curry :  (('a, 'b, 'c) t -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry : ('a -> 'b -> 'c -> 'd) -> ('a, 'b, 'c) t -> 'd
  val compare :
    cmp1:('a -> 'a -> int)
    -> cmp2:('b -> 'b -> int)
    -> cmp3:('c -> 'c -> int)
    -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  val get3 : (_, _, 'a) t -> 'a
  val map1 : f:('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val map2 : f:('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map3 : f:('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
end

(* These functors allow users to write:
   module Foo = struct
     type t = String.t * Int.t
     include Tuple.T2.Comparable (String.t) (Int)
     include Tuple.T2.Hashable (String.t) (Int)
   end
*)

module Make (T1 : sig type t end) (T2 : sig type t end) : sig type t = T1.t * T2.t end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type t := t
end

module Comparable (S1 : Comparable_sexpable) (S2 : Comparable_sexpable)
  : Comparable_sexpable with type t := Make (S1) (S2).t

module type Hashable_sexpable = sig
  type t
  val compare : t -> t -> int
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
end

module Hashable (S1 : Hashable_sexpable) (S2 : Hashable_sexpable)
  : Hashable_sexpable with type t := Make (S1) (S2).t

module Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S)
  : Sexpable.S with type t := Make (S1) (S2).t
