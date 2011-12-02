Basic use of Variantslib:

This code:

    type 'a t =
    | A of 'a
    | B of char
    | C
    | D of int * int
    with variants

generates the following values:

    (* first-class constructor functions *)
    val a : 'a -> 'a t
    val b : char -> 'a t
    val c : 'a t
    val d : int -> int -> 'a t

    (* higher order variants and functions over all variants *)
    module Variants : sig
      val a : ('a -> 'a t)         Variant.t
      val b : (char -> 'a t)       Variant.t
      val c : ('a t)               Variant.t
      val d : (int -> int -> 'a t) Variant.t

      val fold :
        init: 'b
        -> a:('b -> ('a -> 'a t)         Variant.t -> 'c)
        -> b:('c -> (char -> 'a t)       Variant.t -> 'd)
        -> c:('d -> ('a t)               Variant.t -> 'e)
        -> d:('e -> (int -> int -> 'a t) Variant.t -> 'f)
        -> 'f

      val iter :
           a: (('a -> 'a t)         Variant.t -> unit)
        -> b: ((char -> 'a t)       Variant.t -> unit)
        -> c: (('a t)               Variant.t -> unit)
        -> d: ((int -> int -> 'a t) Variant.t -> unit)
        -> unit

      val map :
        'a t
        -> a: (('a -> 'a t)         Variant.t -> 'a                 -> 'r)
        -> b: ((char -> 'a t)       Variant.t -> char               -> 'r)
        -> c: (('a t)               Variant.t                       -> 'r)
        -> d: ((int -> int -> 'a t) Variant.t -> int -> int -> 'a t -> 'r)
        -> 'r
    end

Variant.t is defined in Variantslib as follows:

    module Variant = struct
      type 'constructor t = {
        name : string;
        (* the position of the constructor in the type definition, starting from 0 *)
        rank : int;
        constructor : 'constructor
      }
    end

The fold, iter, and map functions are useful in dealing with the totality of variants.
For example, to get a list of all variants when all the constructors are nullary:

    type t =
    | First
    | Second
    | Third
    with variants

    let all =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants.fold ~init:[]
        ~first:add
        ~second:add
        ~third:add

Just like with pa_fields, if the type changes, the compiler will complain until this
definition is updated as well.
