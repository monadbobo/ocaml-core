(** Library for lazily constructing error messages.  Error messages are intended to
    be constructed in the following style; for simple errors, you write:

    {[Error.of_string "Unable to find file"]}

    For errors where you want to attach some content, you would write:

    {[Error.arg "Unable to find file" <:sexp_of<string>> filename]}

    Or even,

    {[Error.arg "price too big" <:sexp_of<float * [`Max of float]>>
        (price,[`Max max_price])
    ]}
*)
open Sexplib

type t
include Sexpable.S with type t := t
include Binable.S  with type t := t

(** [to_sexp_hum t] returns [t] formatted as sexp that aims to be readable.  This loses
    information about the structure of the error, so if the result is converted back using
    [t_of_sexp], it will not be able to pretty print as nicely.  That is, the following
    equalities do not necessarily hold:

    to_sexp_hum   (t_of_sexp (to_sexp_hum t)) = to_sexp_hum t
    to_string_hum (t_of_sexp (to_sexp_hum t)) = to_string_hum t *)
val to_sexp_hum : t -> Sexp.t

(** might be an expensive operation *)
val to_string_hum : t -> string

val of_string : string -> t

(** Be careful that the body of the lazy or thunk does not access mutable data, since it
    will only be called at an undetermined later point. *)
val of_lazy  : string Lazy.t    -> t
val of_thunk : (unit -> string) -> t

(** Used for creating errors with arguments.  Be careful to use only immutable types as
    the actual argument. *)
val create     : string -> 'a -> ('a -> Sexp.t) -> t
val arg        : string -> ('a -> Sexp.t) -> 'a -> t
val string_arg : string -> ('a -> string) -> 'a -> t

(** Functions for transforming errors *)

(* Add a string to the front of an error *)
val tag : t -> string -> t

(* Add a string and some other data in the form of an s-expression in front of an error *)
val tag_arg : t -> string -> ('a -> Sexp.t) -> 'a -> t

(* Combine multiple errors into one *)
val of_list : ?trunc_after:int -> t list -> t

val of_exn : exn -> t

(* Note that the exception holds onto the [t]. *)
val raise : t -> _
