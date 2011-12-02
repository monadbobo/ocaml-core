open Core.Std

(** Various system utility functions. *)

(** Get the default editor (program) for this user.
    This functions checks the EDITOR and VISUAL environment variables and then
    looks into a hard coded list of editors.
*)
val get_editor : unit -> string option

val get_editor_exn : unit -> string

(** Analogous to [get_editor], defaulting to a list including less and more. *)
val get_pager : unit -> string option

(** [page_contents ?pager ?tmp_name str] will show str in a pager. The optional
    [tmp_name] parameter gives the temporary file prefix. The temporary file is removed
    after running. *)
val page_contents :
  ?pager:string
  -> ?pager_options:string list
  -> ?tmp_name:string
  -> string
  -> unit

val pid_alive : int -> bool

val get_groups : string -> string list

(** [with_tmp pre suf f] creates a temp file, calls f with the file name, and removes the
    file afterwards.  *)
val with_tmp : pre:string -> suf:string -> (string -> 'a) -> 'a

(**
   [checked_edit ~check file]

   Edit a file in safe way, like [vipw(8)]. Launches your default editor on
   [file] and uses [check] to check its content.

   @param check a function returning a text representing the error in the file.
   @param create create the file if it doesn't exists. Default [true]
   @return [`Ok] or [`Abort]. If [`Abort] is returned the files was not modified
   (or created).
*)
val checked_edit :
  ?create:bool
  -> check:(string -> string option)
  -> string
  -> [ `Abort | `Ok ]

(** Edit files containing sexps. *)
module Sexp_checked_edit (S:Sexpable): sig
  val check : string -> string option
  val check_sexps : string -> string option

  val edit :
    ?create:bool
    -> string
    -> [ `Abort | `Ok ]

  val edit_sexps :
    ?create:bool
    -> string
    -> [ `Abort | `Ok ]

end
