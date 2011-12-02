(** Interface to Linux-specific system calls *)
INCLUDE "config.mlh"
IFDEF LINUX_EXT THEN

(** {2 sysinfo} *)

module Sysinfo : sig
  (** Result of sysinfo syscall (man 2 sysinfo) *)
  type t =
    { uptime : Span.t;  (** time since boot *)
      load1 : int;  (** load average over the last minute *)
      load5 : int;  (** load average over the last 5 minutes*)
      load15 : int;  (** load average over the last 15 minutes *)
      total_ram : int;  (** total usable main memory *)
      free_ram : int;  (** available memory size *)
      shared_ram : int;  (** amount of shared memory *)
      buffer_ram : int;  (** memory used by buffers *)
      total_swap : int;  (** total swap page size *)
      free_swap : int;  (** available swap space *)
      procs : int;  (** number of current processes *)
      totalhigh : int;  (** Total high memory size *)
      freehigh : int;  (** Available high memory size *)
      mem_unit : int;  (** Memory unit size in bytes *)
    } with sexp, bin_io

  val sysinfo : unit -> t
end

(** {2 Filesystem functions} *)

(** [sendfile ?pos ?len ~fd sock] sends mmap-able data from file
    descriptor [fd] to socket [sock] using offset [pos] and length [len].
    @return the number of characters actually written.

    NOTE: if the returned value is unequal to what was requested (=
    the initial size of the data by default), the system call may have
    been interrupted by a signal, the source file may have been truncated
    during operation, or a timeout occurred on the socket during sending.
    It is currently impossible to find out which of the events above
    happened.  Calling {!sendfile} several times on the same descriptor
    that only partially accepted data due to a timeout will eventually
    lead to the unix error [EAGAIN].

    @raise Unix_error on Unix-errors.

    @param default pos = 0
    @param default len = length of data (file) associated with descriptor [fd]
*)
val sendfile : ?pos : int -> ?len : int -> fd : Core_unix.File_descr.t -> Core_unix.File_descr.t -> int

(** {2 Non-portable TCP-functionality} *)

type tcp_bool_option = TCP_CORK with sexp, bin_io

(** [gettcpopt_bool sock opt] @return the current value of the boolean
    TCP socket option [opt] for socket [sock]. *)
val gettcpopt_bool : Core_unix.File_descr.t -> tcp_bool_option -> bool

(** [settcpopt_bool sock opt v] sets the current value of the boolean
    TCP socket option [opt] for socket [sock] to value [v]. *)
val settcpopt_bool : Core_unix.File_descr.t -> tcp_bool_option -> bool -> unit

(** [send_nonblocking_no_sigpipe sock ?pos ?len buf] tries to do a
    nonblocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error
    in that case immediately.  @return [Some bytes_written] or [None]
    if the operation would have blocked.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise Invalid_argument if the designated buffer range is invalid.
    @raise Unix_error on Unix-errors.
*)
val send_nonblocking_no_sigpipe :
  Core_unix.File_descr.t -> ?pos : int -> ?len : int -> string -> int option

(** [send_no_sigpipe sock ?pos ?len buf] tries to do a
    blocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error in
    that case immediately.  @return the number of bytes written.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise Invalid_argument if the designated buffer range is invalid.
    @raise Unix_error on Unix-errors.
*)
val send_no_sigpipe :
  Core_unix.File_descr.t -> ?pos : int -> ?len : int -> string -> int

(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] tries to do
    a nonblocking send on socket [sock] using [count] I/O-vectors
    [iovecs].  Prevents [SIGPIPE], i.e. raises a Unix-error in that
    case immediately.  @return [Some bytes_written] or [None] if the
    operation would have blocked.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)
val sendmsg_nonblocking_no_sigpipe :
  Core_unix.File_descr.t -> ?count : int -> string Core_unix.IOVec.t array -> int option

(** {2 Clock functions} *)

module Clock : sig
  type t

  (* All these functions can raise Unix_error. *)

  (* returns the CPU-clock associated with the thread *)
  val get : Thread.t -> t

  val get_time : t -> Span.t

  val set_time : t -> Span.t -> unit

  val get_resolution : t -> Span.t

  (** [get_process_clock] the clock measuring the CPU-time of a process. *)
  val get_process_clock : unit -> t

  (** [get_thread_clock] the clock measuring the CPU-time of the current
      thread. *)
  val get_thread_clock : unit -> t
end

(** {2 Parent death notifications} *)

(** [pr_set_pdeathsig s] sets the signal [s] to be sent to the executing
    process when its parent dies.  NOTE: the parent may have died
    before or while executing this system call.  To make sure that you
    do not miss this event, you should call {!Core_unix.getppid} to get
    the parent process id after this system call.  If the parent has
    died, the returned parent PID will be 1, i.e. the init process will
    have adopted the child.  You should then either send the signal to
    yourself using Unix.kill, or execute an appropriate handler. *)
val pr_set_pdeathsig : Signal.t -> unit

(** [pr_get_pdeathsig ()] get the signal that will be sent to the
    currently executing process when its parent dies. *)
val pr_get_pdeathsig : unit -> Signal.t


(** {2 Task name} *)

(** [pr_set_name_first16 name] sets the name of the executing thread to [name].  Only
    the first 16 bytes in [name] will be used, the rest is ignored. *)
val pr_set_name_first16 : string -> unit

(** [pr_get_name ()] gets the name of the executing thread.  The name is
    at most 16 bytes long. *)
val pr_get_name : unit -> string


(** {2 Pathname resolution} *)

(** [file_descr_realpath fd] @return the canonicalized absolute
    pathname of the file associated with file descriptor [fd].

    @raise Unix_error on errors.
*)
val file_descr_realpath : Core_unix.File_descr.t -> string

(** [out_channel_realpath oc] @return the canonicalized absolute
    pathname of the file associated with output channel [oc].

    @raise Unix_error on errors.
*)
val out_channel_realpath : out_channel -> string

(** [in_channel_realpath ic] @return the canonicalized absolute
    pathname of the file associated with input channel [ic].

    @raise Unix_error on errors.
*)
val in_channel_realpath : in_channel -> string

(** {2 Affinity} *)

(* Setting the CPU affinity causes a process to only run on the
   cores chosen.  You can find out how many cores a system has in
   /proc/cpuinfo.  This can be useful in two ways: first, it limits
   a process to a core so that it won't interfere with processes on
   other cores.  Second, you save time by not moving the process back
   and forth between CPUs, which sometimes invalidates their cache.
   See "man sched_setaffinity" for details. *)
val sched_setaffinity : ?pid : Pid.t -> cpuset : int list -> unit -> unit


(** [cores ()] @return the number of cores on the machine *)
val cores : unit -> int
ENDIF
