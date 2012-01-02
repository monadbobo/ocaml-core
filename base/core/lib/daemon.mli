(** [daemonize ?(close_stdio = true) ?(cd = "/") ?umask=[0] ()] makes the current
    executing process a daemon, and dups /dev/null to stdin/stdout/stderr if
    close_stdio=true. See Chapter 13 of Advanced Programming in the UNIX Environment
    Second Edition by Stephens and Rago for more details.

    @raise Failure if fork was unsuccessful.
*)
val daemonize :
  ?close_stdio : bool
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> unit

(** [daemonize_wait ?(cd = "/") ?(umask=0) ()] makes the executing process a daemon, but
    delays full detachment from the calling shell/process until the returned [release]
    function is called.  Any output to stdout/stderr before [release] is called will get
    sent out normally.  After [release] is called, /dev/null gets dup'd to
    stdin/stdout/stderr.  If the process exits before [release] is called, the exit code
    will bubble up to the calling shell/process.

    Note that calling [release] will adjust SIGPIPE handling, so you should not rely on
    the delivery of this signal during this time.

    [daemonize_wait] allows you to daemonize and then start async, but still have
    stdout/stderr go to the controlling terminal during startup.  By default, when you
    [daemonize], toplevel exceptions during startup would get sent to /dev/null.  With
    [daemonize_wait], toplevel exceptions can go to the terminal until you call [release].

    @raise Failure if fork was unsuccessful.
*)
val daemonize_wait : ?cd : string -> ?umask : int -> unit -> (unit -> unit)
