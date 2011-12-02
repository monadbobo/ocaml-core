open Core.Std

(**
   Extensions to [Core.Core_String] .
*)

(**
   [collate s1 s2] sorts string in an order that's is usaully more suited
   for human consumption by treating ints specificaly:
   (e.g. it will output: [["rfc1.txt";"rfc822.txt";"rfc2086.txt"]]).

   It works by splitting the strings in numerical and non numerical chunks and
   comparing chunks two by two from left to right (and starting on a non
   numerical chunks):
   - Non_numerical chunks are compared using lexicographical ordering.
   - Numerical chunks are compared based on the values of the represented ints
   and the number of trailing zeros.

   It is a total order.
*)
val collate : string -> string -> int

(**
   [unescaped s] is the inverse operation of [escaped]: it takes a string where
   all the special characters are escaped following the lexical convention of
   OCaml and returns an unescaped copy.
   The [strict] switch is on by default and makes the function treat illegal
   backslashes as errors.
   When [strict] is [false] every illegal backslash except escaped numeral
   greater than [255] is copied literally. The aforementioned numerals still
   raise errors. This mimics the behaviour of the ocaml lexer.
*)
val unescaped : ?strict:bool -> string -> string

(**
   Same as [unescaped] but instead of raising [Failure _] returns an error
   message with the position in the string in case of failure.
*)
val unescaped_res : ?strict:bool -> string -> (string,(int*string)) Core.Result.t

(** [squeeze str] reduces all sequences of spaces, newlines, tables, and
 * carriage returns to single spaces.
 *)
val squeeze : string -> string

(** [is_substring ~substring t] returns [true] if substring is a substring
 * of t.
 *)
val is_substring : substring:string -> string -> bool

(** [pad_left ~char s len]
    Returns [s] padded to the length [len] by adding characters [char] to the
    left of the string. If s is already longer than [len] it is returned unchanged.
*)
val pad_left : ?char:char -> string -> int -> string
val pad_right : ?char:char -> string -> int -> string

(**deprecated in favour of word_wrap *)
val line_break: len:int -> string -> string list

(**
   [word_wrap ~soft_limit s]

   Wraps the string so that it fits the length [soft_limit]. It doesn't break
   words unless we go over [hard_limit].

   if [nl] is passed it is inserted instead of the normal newline character.
*)
val word_wrap:
  ?trailing_nl:bool
  -> ?soft_limit:int
  -> ?hard_limit:int
  -> ?nl:string
  -> string
  -> string

module Escaping : sig
  (**
     String escaping.

     Operations for escaping and unescaping strings, with paramaterized escape
     and escapeworthy characters.
  *)

  (** [escape_gen escapeworthy_map escape_char s] returns an escaped string based on
      [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then all occurences of
      [c1] are replaced by [escape_char] concatenated to [c2]. *)
  val escape_gen :
    escapeworthy_map:(char * char) list -> escape_char:char -> string -> string

  (** [escape escapeworthy escape_char s] is
      [escape_gen ~escapeworthy_map:(List.zip_exn escapeworthy escapeworthy)
      ~escape_char]. *)
  val escape : escapeworthy:char list -> escape_char:char -> string -> string

  (** [escape_one_orig ~escapeworthy ~escape_char s] escapes character
      [escapeworthy] with [escape_char] in string [s].  The function
      returns the original string if no character had to be escaped. *)
  val escape_one_orig :
    escapeworthy : char -> escape_char : char -> string -> string

  (** [escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char s]
      escapes characters [escapeworthy1] and [escapeworthy2] with
      [escape_char] in string [s].  The function returns the original
      string if no character had to be escaped. *)
  val escape_two_orig :
    escapeworthy1 : char -> escapeworthy2 : char -> escape_char : char -> string
    -> string

  (** [unescape_gen] is the inverse operation of [escape_gen], assuming an inverse
      map is given.  That is, [unescape_gen map escape_char s] returns an escaped string
      based on [s] as follows: if [(c1,c2)] is in [map], then all occurrences of
      [escape_char][c1] are replaced by [c2]. *)
  val unescape_gen :
    map:(char * char) list -> escape_char:char -> string -> string

  (** [unescape escape_char s] is [unescape_gen ~map:\[\] ~escape_char str] *)
  val unescape : escape_char:char -> string -> string

  (** [is_escaped escape_char s pos] return true if the char at pos is escaped,
      false otherwise. *)
  val is_char_escaped : escape_char:char -> string -> int -> bool

  (** [is_literal escape_char s pos] return true if the char at pos is not escaped
      (literal). *)
  val is_char_literal : escape_char:char -> string -> int -> bool

  (** [index escape_char s char] find the first literal (not escaped) instance of
      char in s starting from 0. *)
  val index : escape_char:char -> string -> char -> int option
  val index_exn : escape_char:char -> string -> char -> int

  (** [rindex escape_char s char] find the first literal (not escaped) instance of
      char in s starting from the end of s and proceeding towards 0. *)
  val rindex : escape_char:char -> string -> char -> int option
  val rindex_exn : escape_char:char -> string -> char -> int

  (** [index_from escape_char s pos char] find the first literal (not escaped)
      instance of char in s starting from pos and proceeding towards the end of s. *)
  val index_from : escape_char:char -> string -> int -> char -> int option
  val index_from_exn : escape_char:char -> string -> int -> char -> int

  (** [rindex_from escape_char s pos char] find the first literal (not escaped)
      instance of char in s starting from pos and towards 0. *)
  val rindex_from : escape_char:char -> string -> int -> char -> int option
  val rindex_from_exn : escape_char:char -> string -> int -> char -> int

  (** [split escape_char s ~on] @return a list of substrings of [s] that are separated by
      literal versions of [on].  Consecutive [on] characters will cause multiple empty
      strings in the result.  Splitting the empty string returns a list of the empty
      string, not the empty list.

      e.g. split ~escape_char:'_' ~on:',' "foo,bar_,baz" = ["foo"; "bar_,baz"]
  *)
  val split : string -> on:char -> escape_char:char -> string list

  (** [split_on_chars s ~on] @return a list of all substrings of [s]
      that are separated by one of the literal chars from [on].  [on]
      are not grouped.  So a grouping of [on] in the source string will
      produce multiple empty string splits in the result.

      e.g. split_on_chars ~escape_char:'_' ~on:[',';'|'] "foo_|bar,baz|0" ->
      ["foo_|bar"; "baz"; "0"]
  *)
  val split_on_chars : string -> on:char list -> escape_char:char -> string list

  (* [lsplit2 s on escape_char] splits s into a pair on the first literal instance
     of [on] (meaning the first unescaped instance) starting from the left. *)
  val lsplit2 : string -> on:char -> escape_char:char -> (string * string) option
  val lsplit2_exn : string -> on:char -> escape_char:char -> (string * string)

  (* [rsplit2 s on escape_char] splits s into a pair on the first literal instance
     of [on] (meaning the first unescaped instance) starting from the right. *)
  val rsplit2 : string -> on:char -> escape_char:char -> (string * string) option
  val rsplit2_exn : string -> on:char -> escape_char:char -> (string * string)
end

(** Consolidates a list of strings (almost [^]) losslessly.  E.g.:

     abc-def-1-ghijk
     abc-def-2-ghijk
     abc-def-5-ghijk
     abc-xyz-2
     abc-xyz-3

   becomes:

     abc-{def-[1-2,5]-ghijk,xyz-[2-3]}

   The algorithm is conceptually as follows:
    1) if all strings are sequences of digits, return [ contiguous subranges ],
    otherwise:
    2) split all strings into groups of consecutive letters or digits (but not both)
    3) break the list into sublists by the 1st and last token (1st token has precedence)
    4) for each sublist, find the longest prefix and suffix common for all entries
    5) replace each sublist with:
    "common_prefix-<result_of_the_recursive_call>-common_suffix"
    6) return { String.concat ~sep:"," <compressed_sublists> }

   In the implementation, we only tokenize the strings once, and then recursively work
    with lists of tokens.

   [^] Repeated entries and the original ordering of the list are not preserved.
   Otherwise, this transformation is lossless.

   Additionally, one may choose to represent sets of integers as the [lower..upper bound]
   (so [1-2,5] becomes [1..5]), or just by an asterisk ("*"), when brevity is preferred
   over accuracy.

*)
val consolidate_strings :
  ?int_sets : [`Exact | `Bounds | `Asterisk]
  -> string list
  -> string


(** Consolidates a list of strings in such a way that the result does not exceed the given
    length.  Tries the following, in order:

    1) plain consolidate_strings
    2) consolidate_strings ~int_sets:`Bounds
    3) consolidate_strings ~int_sets:`Asterisk
    4) prefix of 3 ^ "..."
*)
val consolidate_strings' :
  max_len:int
  -> string list
  -> string

