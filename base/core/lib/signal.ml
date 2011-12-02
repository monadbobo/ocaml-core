module Int = Core_int
module List = Core_list
module Hashtbl = Core_hashtbl

let failwithf = Core_printf.failwithf

type t = int

include (Int : sig
  include Comparable.S with type t := t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
end)

external ml_caml_to_nonportable_signal_number : int -> int =
  "ml_caml_to_nonportable_signal_number"

external ml_nonportable_to_caml_signal_number : int -> int =
  "ml_nonportable_to_caml_signal_number"

let of_system_int t = ml_nonportable_to_caml_signal_number t
let to_system_int t = ml_caml_to_nonportable_signal_number t

let of_caml_int t = t
let to_caml_int t = t

type sys_behavior = Continue | Dump_core | Ignore | Stop | Terminate with sexp

let equal (t : t) t' = (t = t')

include struct
  (* Please keep in sync with the list for to_string/sys_behavior *)
  open Sys
  let abrt = sigabrt
  let alrm = sigalrm
  let chld = sigchld
  let cont = sigcont
  let fpe = sigfpe
  let hup = sighup
  let ill = sigill
  let int = sigint
  let kill = sigkill
  let pipe = sigpipe
  let prof = sigprof
  let quit = sigquit
  let segv = sigsegv
  let stop = sigstop
  let term = sigterm
  let tstp = sigtstp
  let ttin = sigttin
  let ttou = sigttou
  let usr1 = sigusr1
  let usr2 = sigusr2
  let vtalrm = sigvtalrm
  let zero = 0
end

let to_string, default_sys_behavior =
  let known =
    [
      ("abrt", abrt, Dump_core);
      ("alrm", alrm, Terminate);
      ("chld", chld, Ignore);
      ("cont", cont, Continue);
      ("fpe", fpe, Dump_core);
      ("hup", hup, Terminate);
      ("ill", ill, Dump_core);
      ("int", int, Terminate);
      ("kill", kill, Terminate);
      ("pipe", pipe, Terminate);
      ("prof", prof, Terminate);
      ("quit", quit, Dump_core);
      ("segv", segv, Dump_core);
      ("stop", stop, Stop);
      ("term", term, Terminate);
      ("tstp", tstp, Stop);
      ("ttin", ttin, Stop);
      ("ttou", ttou, Stop);
      ("usr1", usr1, Terminate);
      ("usr2", usr2, Terminate);
      ("vtalrm", vtalrm, Terminate);
      ("<zero>", zero, Ignore);
    ]
  in
  let str_tbl = Int.Table.create ~size:1 () in
  let behavior_tbl = Int.Table.create ~size:1 () in
  List.iter known ~f:(fun (name, s, behavior) ->
    Hashtbl.replace str_tbl ~key:s ~data:("sig" ^ name);
    Hashtbl.replace behavior_tbl ~key:s ~data:behavior);
  (* For unknown signal numbers, [to_string] returns a meaningful
     string, while [default_sys_behavior] has to raise an exception
     because we don't know what the right answer is. *)
  let to_string s =
    match Hashtbl.find str_tbl s with
    | None -> "<unknown signal>"
    | Some string -> string
  in
  let default_sys_behavior s =
    match Hashtbl.find behavior_tbl s with
    | None ->
        raise (Invalid_argument "Signal.default_sys_behavior: unknown signal")
    | Some behavior -> behavior
  in
  to_string, default_sys_behavior
;;

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] ;;

let pid_spec_to_string = function
  | `Pid pid   -> Pid.to_string pid
  | `My_group  -> "0"
  | `Group pid -> Int.to_string (- (Pid.to_int pid))
;;

let send signal pid_spec =
  try
    let kill =
      match pid_spec with
      | `Pid pid    -> UnixLabels.kill ~pid:(Pid.to_int pid)
      | `My_group   -> UnixLabels.kill ~pid:0
      | `Group pgrp -> UnixLabels.kill ~pid:(- (Pid.to_int pgrp))
    in
    kill ~signal;
    `Ok
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> `No_such_process
;;

let send_i t pid_spec =
  match send t pid_spec with
  | `Ok | `No_such_process -> ()
;;

let send_exn t pid_spec =
  match send t pid_spec with
  | `Ok -> ()
  | `No_such_process ->
      failwithf "Signal.send_exn %s pid:%s" (to_string t)
        (pid_spec_to_string pid_spec) ()
;;

type behavior = [ `Default | `Ignore | `Handle of t -> unit ]

module Behavior = struct
  type t = behavior

  let of_caml = function
    | Sys.Signal_default -> `Default
    | Sys.Signal_ignore -> `Ignore
    | Sys.Signal_handle f -> `Handle f

  let to_caml = function
    | `Default -> Sys.Signal_default
    | `Ignore -> Sys.Signal_ignore
    | `Handle f -> Sys.Signal_handle f
end

let signal t behavior =
  Behavior.of_caml (Sys.signal t (Behavior.to_caml behavior))
;;

let set t behavior = ignore (signal t behavior)

let handle t f = set t (`Handle f)
let handle_default t = set t `Default
let ignore t = set t `Ignore

type sigprocmask_command = [ `Set | `Block | `Unblock ]

let sigprocmask mode sigs =
  let mode =
    match mode with
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
    | `Set -> Unix.SIG_SETMASK
  in
  Unix.sigprocmask mode sigs
;;

let sigpending = Unix.sigpending
let sigsuspend = Unix.sigsuspend
