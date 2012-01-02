
module List_math : sig
  (** [frange] is similar to [List.range], but for floats. *)
  val frange : ?stride:float -> float -> float -> float list

  (** [linspace] is similar to [frange], but it takes the number of elements in the output
      as an argument, rather than the size of the stride, which is more numerically
      robust.  The [endpoint] parameter explicitly controls whether [stop] value should be
      included in the output (the default) or not.

      This function is a clone of [numpy.linspace].
  *)
  val linspace: ?endpoint:bool -> float -> float -> int -> float list

  (** [sum] sum a list of floats.  Uses the Kahan summation algorithm. *)
  val sum : float list -> float

  (** [sum_sq] sum the squares of a list of floats  *)
  val sum_sq : float list -> float

  (** [sum_product_exn] compute the sum of element-wise product of two lists of floats *)
  val sum_product_exn : float list -> float list -> float
end

module Array_math : sig
  (* Same functions as [List_math].  [frange] and [linspace] are implmented in terms of
     lists and then converted.  [sum], [sum_sq], and [sum_product_exn] are implemented
     directly in terms of arrays and the list functions are the ones that have to
     convert. *)
  val frange : ?stride:float -> float -> float -> float array
  val linspace: ?endpoint:bool -> float -> float -> int -> float array
  val sum : float array -> float
  val sum_sq : float array -> float
  val sum_product_exn : float array -> float array -> float
end
