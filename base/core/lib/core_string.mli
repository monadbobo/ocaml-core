(** An extension of the standard StringLabels. If you open Core.Std, you'll get
    these in the String module. *)

type t = string

include Binable.S with type t := t
include Comparable.S_binable with type t := t
include Container.S0 with type t := t with type elt = char
include Hashable.S_binable with type t := t
include Sexpable.S with type t := t
include Stringable.S with type t := t

(** Maximum length of a string. *)
val max_length : int

(* From the standard StringLabels *)
external length : t -> int = "%string_length"

external get : t -> int -> char = "%string_safe_get"
external set : t -> int -> char -> unit = "%string_safe_set"

external create : int -> t = "caml_create_string"
val make : int -> char -> t
val copy : t -> t
val init : int -> f:(int -> char) -> t

val sub : t -> pos:int -> len:int -> t

val fill : t -> pos:int -> len:int -> char -> unit

val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

(** concatanate all strings in the list using separator [sep] (default sep "") *)
val concat : ?sep:t -> t list -> t

(* (** Like concat, but uses the Container typeclass *)
val tc_concat : (t, 'container) Container.tc -> sep:t -> 'container -> t *)

(** Warning: Only returns a copy if changes are necessary!  Special characters are
    represented by escape sequences, following the lexical conventions of Objective
    Caml. *)
val escaped : t -> t

val contains : ?pos:int -> ?len:int -> t -> char -> bool
val contains_from  : t -> int -> char -> bool
val rcontains_from : t -> int -> char -> bool

val uppercase : t -> t
val lowercase : t -> t

val capitalize   : t -> t
val uncapitalize : t -> t

val index : t -> char -> int option
val index_exn : t -> char -> int

val rindex : t -> char -> int option
val rindex_exn : t -> char -> int

val index_from : t -> int -> char -> int option
val index_from_exn : t -> int -> char -> int

val rindex_from : t -> int -> char -> int option
val rindex_from_exn : t -> int -> char -> int

(** [slice s start stop] gets a slice of [s] between [start] and [stop].
    [start] and [stop] will be normalized before the access.
    (viz. Core_array.normalize). *)
val slice : t -> int -> int -> t

(** Returns the reversed list of characters contained in a list. *)
val to_list_rev : t -> char list

(** [nget s i] Gets the char at normalized position [i] in [s]. *)
val nget : t -> int -> char

(** [nset s i c] Sets the char at normalized position [i] to [c]. *)
val nset : t -> int -> char -> unit

(** [is_suffix s ~suffix] returns [true] if [s] ends with [suffix]. *)
val is_suffix : t -> suffix:t -> bool

(** [is_prefix s ~prefix] returns [true] if [s] starts with [prefix]. *)
val is_prefix : t -> prefix:t -> bool

(** If the string [s] contains the character [on], then [lsplit2_exn
    s ~on] returns a pair containing [s] split around the first
    appearance of [on] (from the left).
    @raise Not_found When [on] cannot be found in [s]
*)
val lsplit2_exn : t -> on:char -> t * t

(** If the string [s] contains the character [on], then [rsplit2_exn
    s ~on] returns a pair containing [s] split around the first
    appearance of [on] (from the right).
    @raise Not_found When [on] cannot be found in [s]
*)
val rsplit2_exn : t -> on:char -> t * t

(** [lsplit2 line ~on] optionally returns [line] split into two strings around the
  * first appearance of [on] from the left *)
val lsplit2 : t -> on:char -> (t * t) option

(** [rsplit2 line ~on] optionally returns [line] split into two strings around the
  * first appearance of [on] from the right *)
val rsplit2 : t -> on:char -> (t * t) option

(** [split s ~on] @return a list of substrings of [s] that are separated by
    [on].  Consecutive [on] characters will cause multiple empty strings
    in the result.  Splitting the empty string returns a list of the empty
    string, not the empty list. *)
val split : t -> on:char -> t list

(** [split_on_chars s ~on] @return a list of all substrings of [s]
    that are separated by one of the chars from [on].  [on]
    are not grouped.  So a grouping of [on] in the source string will
    produce multiple empty string splits in the result.  *)
val split_on_chars : t -> on:char list -> t list

(** [lfindi s ~f] returns the index [i] of the first character in [s]
    satisfying [f i s.[i]]. *)
val lfindi : t -> f:(int -> char -> bool) -> int option

(** [rfindi s ~f] returns the index [i] of the last character in [s]
    satisfying [f i s.[i]]. *)
val rfindi : t -> f:(int -> char -> bool) -> int option

(* Warning: the following strip functions have copy-on-write semantics (i.e. they may
   return the same string passed in) *)

(** [lstrip s] returns a string with consecutive white space (tabs,
    spaces, newlines, and carriage returns) stripped from the beginning of
    [s]. *)
val lstrip : t -> t

(** [rstrip s] returns a string with consecutive white space (tabs,
    spaces, newlines, and carriage returns) stripped from the end of
    [s]. *)
val rstrip : t -> t

(** [strip s] returns a string with consecutive white space (tabs,
    spaces, newlines, and carriage returns) stripped from the beginning
    and end of [s]. *)
val strip : t -> t

(** [map f s] applies [f] to each character in [s], and returns the
    resulting string. *)
val map : t -> f : (char -> char) -> t

(** [mapi f s] applies [f] to each character in [s] and its index, and returns the
    resulting string. *)
val mapi : t -> f : (int -> char -> char) -> t

(** Like [map], but allows replacement of a single character with zero or two or more
    characters. *)
val concat_map : ?sep:t -> t -> f : (char -> t) -> t

(** [tr target replacement s] replaces every instance of [target] in [s] with
    [replacement]. *)
val tr : target : char -> replacement : char -> t -> t

(** [tr_inplace target replacement s] destructively modifies s (in place!)
    replacing every instance of [target] in [s] with [replacement]. *)
val tr_inplace : target : char -> replacement : char -> t -> unit

(** [chop_suffix s ~suf] returns a copy [s] without the trailing [suff]
    @raise Invalid_argument is [suff] is not a suffix of [s]
*)
val chop_suffix_exn : t -> suffix:t -> t

(** [chop_prefix s ~pref] returns a copy [s] without the leading [pref]
    @raise Invalid_argument is [pref] is not a prefix of [s]
*)
val chop_prefix_exn : t -> prefix:t -> t

val chop_suffix : t -> suffix:t -> t option

val chop_prefix : t -> prefix:t -> t option

(** [suffix s n] returns the longest suffix of [s] of length less than or equal to [n] *)
val suffix : t -> int -> t

(** [prefix s n] returns the longest prefix of [s] of length less than or equal to [n] *)
val prefix : t -> int -> t

(** [drop_suffix s n] drops the longest suffix of [s] of length less than or equal to [n] *)
val drop_suffix : t -> int -> t

(** [drop_prefix s n] drops the longest prefix of [s] of length less than or equal to [n] *)
val drop_prefix : t -> int -> t

(** [concat_array sep ar] like {!String.concat}, but operates on arrays *)
val concat_array : ?sep : t -> t array -> t

(** slightly faster hash function on strings *)
val hash : t -> int

(** fast equality function on strings, doesn't use compare_val *)
val equal : t -> t -> bool

(** This has to be public for interactive top-levels. *)
val pp : Format.formatter -> t -> unit

(** [is_empty s] returns [true] iff [s] is empty (i.e. its length is 0). *)
val is_empty : t -> bool

module Infix : sig
  val ( </> ) : t -> int * int -> t
end

val of_char : char -> t
