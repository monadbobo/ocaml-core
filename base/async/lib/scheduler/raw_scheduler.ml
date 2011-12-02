open Core.Std
open Import

module Block_group = Async_core.Block_group
module Work = Block_group.Work

module Signal = Core.Std.Signal

let syscall = Syscall.syscall

let debug = Debug.debug

let upon = Deferred.upon

module File_descr = Unix.File_descr

module Fd = Raw_fd

type finalizer_job = Execution_context.t * (unit -> unit)

type fd_by_descr = (File_descr.t, Fd.t) Bounded_int_table.t with sexp_of

type t =
  {
    (* The scheduler [mutex] must be locked by all code that is manipulating scheduler
       data structures, which is almost all async code (including things like [upon] and
       [bind]).  The [mutex] is automatically locked in the main thread when the
       scheduler is first created.

       A [Nano_mutex] keeps track of which thread is holding the lock.  This means we
       can detect errors in which code is trying to acquire the async lock while it
       already holds it, or release the lock when it doesn't. *)
    mutex : Nano_mutex.t;

    mutable go_has_been_called : bool;

    file_descr_watcher : Fd.t File_descr_watcher.t;

    (* [fd_by_descr] holds every file descriptor that Async knows about.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : fd_by_descr;

    (* The following three fields are used to interrupt async when it is blocked in a
       call to [select].  The interrupt is done by writing a byte to a pipe whose
       file_descr is one of the read file_descrs passed to select.

       [am_in_select_do_cycle] keeps track of whether async is currently in the middle
       of a cycle running jobs, and is used to avoid doing the write in the case when it
       is, since the select will already get a chance to run when the cycle finishes.

       [already_interrupted_select] keeps track of whether we've already interrupted
       async during this call to [select], and is used to avoid doing the write multiple
       times for a given call.  [already_interrupted_select] does not exactly track the
       state of the pipe.  It is possible for [already_interrupted_select] to be false
       and for the pipe to be nonempty.  The key property is that if
       [already_interrupted_select] is true then the pipe is nonempty. *)
    mutable am_in_select_do_cycle : bool;
    mutable already_interrupted_select : bool;
    interrupt_select_pipe : Fd.t Read_write.t;

    (* For signals that async is handling, when a signal arrives, the OCaml signal
       handler sets [signal_was_delivered] to [true] and records the signal in the [bool
       ref] associated with the signal in [delivered_signals].  The select loop checks
       if a [signal_was_delivered], and if so for each signal that was delivered, clears
       the [bool ref] and extends [signals] with the signal.  Async programs that handle
       signals iterate over the stream of [signals] to get notified of signal delivery.

       [signal_was_delivered] is not necessary, but is an optimization so that the
       select loop only has to iterate over [delivered_signals] if a signal was
       delivered, and if not can skip dealing with signals entirely.

       [delivered_signals] is created during async startup and is not modified after
       that, since Hashtbls aren't thread safe.  Only the [bool ref] is modified
       by signal handlers and the select loop. *)
    signals : Signal.t Tail.t;
    mutable signal_was_delivered : bool;
    delivered_signals : bool ref Signal.Table.t;

    (* Finalizers are very much like signals; they can come at any time and in any
       thread.  So, when an OCaml finalizer fires, we stick a closure to do the work
       in a thread-safe queue of [finalizer_jobs], which the select loop then schedules
       to run as ordinary async jobs. *)
    finalizer_jobs : finalizer_job Thread_safe_queue.t sexp_opaque;

    (* [num_blocked_threads] is the sum over all block groups of num_blocked_threads for
       that block group. *)
    mutable num_blocked_threads : int;
    mutable num_live_threads : int;
    (* [num_reserved_threads] is the sum over all block groups of num_reserved_threads
       for that block group. *)
    mutable num_reserved_threads : int;
    max_num_live_threads : int;
    work_for_threads: Work.t Squeue.t;
  }
with fields, sexp_of

let lock t =
  (* The following debug message is outside the lock, and so there can be races between
     multiple threads printing this message. *)
  if debug then Debug.print "waiting on lock";
  Nano_mutex.lock_exn t.mutex;
;;

let try_lock t =
  match Nano_mutex.try_lock_exn t.mutex with
  | `Acquired -> true
  | `Not_acquired -> false
;;

let unlock ?allow_from_any_thread t =
  if debug then Debug.print "lock released";
  Nano_mutex.unlock_exn t.mutex ?allow_from_any_thread;
;;

let with_lock t f =
  lock t;
  Core.Std.protect ~f ~finally:(fun () -> unlock t);
;;

type the_one_and_only =
| Not_ready_to_initialize
| Ready_to_initialize of (unit -> t)
| Initialized of t

(* We use a mutex to protect creation of the one-and-only scheduler in the event that
   multiple threads attempt to call [the_one_and_only] simultaneously, which can
   happen in programs that are using [In_thread.run_in_async]. *)
let mutex_for_initializing_the_one_and_only_ref = Mutex.create ()
let the_one_and_only_ref : the_one_and_only ref = ref Not_ready_to_initialize

(* Handling the uncommon cases in this function allows [the_one_and_only] to be
   inlined.  The presence of a string constant keeps this function from being
   inlined. *)
let the_one_and_only_uncommon_case ~should_lock =
  Mutex.critical_section mutex_for_initializing_the_one_and_only_ref ~f:(fun () ->
    match !the_one_and_only_ref with
    | Initialized t -> t
    | Not_ready_to_initialize ->
      failwith "Async the_one_and_only not ready to initialize"
    | Ready_to_initialize f ->
      let t = f () in
      (* We lock the scheduler because the user may do Async stuff at the top level
         before calling [Scheduler.go], and we don't want anyone to be able to run jobs
         until [Scheduler.go] is called.  This could happen, e.g. by creating a reader
         that does a read system call in another (true) thread.  The scheduler will
         remain locked until the select loop called by [Scheduler.go] unlocks it. *)
      if should_lock then lock t;
      the_one_and_only_ref := Initialized t;
      t)
;;

let the_one_and_only ?(should_lock = true) () =
  match !the_one_and_only_ref with
  | Initialized t -> t
  | Not_ready_to_initialize | Ready_to_initialize _ ->
    the_one_and_only_uncommon_case ~should_lock
;;

(* [thread_safe_interrupt_select] is called when the select loop needs to be woken up to
   process changes, for any of the following reasons:

   * to start watching a file descriptor
   * to add a timer event
   * to process a finalizer
   * to process a signal
   * to process a toplevel unhandled exception

   As the name implies, it is safe to call from any thread;
   [thread_safe_interrupt_select] does not assume the scheduler lock is held, although
   it is fine if it is.  Because of OCaml's compilation, the test-and-set of
   [t.already_interrupted_select] is atomic, so we will only ever write one byte to the
   pipe before it is cleared. *)
let thread_safe_interrupt_select t =
  if debug then Debug.print "thread_safe_interrupt_select";
  (* BEGIN ATOMIC *)
  if not t.already_interrupted_select then begin
    t.already_interrupted_select <- true;
    (* END ATOMIC *)
    if debug then Debug.print "writing to interrupt_select_pipe_write";
    Fd.syscall_exn (Read_write.get t.interrupt_select_pipe `Write) ~nonblocking:true
      (fun file_descr ->
        let module U = Unix in
        try
          ignore (U.write_assume_fd_is_nonblocking file_descr "w")
        with
        | U.Unix_error ((U.EWOULDBLOCK | U.EAGAIN), _, _) -> ())
  end
;;

let am_in_async t = Nano_mutex.current_thread_has_lock t.mutex

let is_main_thread () = Thread.id (Thread.self ()) = 0

let remove_fd t fd = Bounded_int_table.remove t.fd_by_descr fd.Fd.file_descr

let maybe_start_closing_fd t fd =
  if fd.Fd.num_active_syscalls = 0 then begin let module S = Fd.State in
    match fd.Fd.state with
    | S.Closed | S.Open | S.Replaced -> ()
    | S.Close_requested close ->
      (* We must remove the fd now and not after the close has finished.  If we waited
         until after the close had finished, then the fd might have already been
         reused by the OS and replaced. *)
      remove_fd t fd;
      Fd.set_state fd S.Closed;
      upon (close ()) (fun () -> Ivar.fill fd.Fd.close_finished ());
  end;
;;

let dec_num_active_syscalls_fd t fd =
  fd.Fd.num_active_syscalls <- fd.Fd.num_active_syscalls - 1;
  maybe_start_closing_fd t fd;
;;

let create_fd fd_by_descr kind file_descr ~name =
  match Bounded_int_table.find fd_by_descr file_descr with
  | Some fd ->
    fail "attempt to overwrite existing fd" (fd, fd_by_descr)
    <:sexp_of< Fd.t * fd_by_descr >>
  | None ->
    let supports_nonblock =
      (* Do not change blocking status of TTYs!  This would affect all processes currently
         attached to that TTY and even persist after this process terminates. *)
      if Core.Std.Unix.isatty file_descr then
        false
      else begin
        let module K = Fd.Kind in
        begin match kind with
        | K.File
          (* No point in setting nonblocking for files.  Unix doesn't care. *)
          -> false
        | K.Char
        | K.Fifo
        (* `Unconnected sockets support nonblocking so we can connect() them.
           `Passive     sockets support nonblocking so we can accept() them.
           `Active      sockets support nonblocking so we can read() and write() them. *)
        | K.Socket (`Unconnected | `Bound | `Passive | `Active)
          -> true
        end
      end
    in
    let fd =
      { Fd.
        name;
        file_descr;
        kind;
        supports_nonblock;
        have_set_nonblock = false;
        state = Fd.State.Open;
        num_active_syscalls = 0;
        close_finished = Ivar.create ();
        ready_to = Read_write.create_both None;
      }
    in
    assert (not (Bounded_int_table.mem fd_by_descr file_descr));
    Bounded_int_table.replace fd_by_descr ~key:file_descr ~data:fd;
    fd;
;;

let invariant t =
  if debug then begin
    try
      assert (0 <= t.num_blocked_threads);
      assert (t.num_blocked_threads <= t.num_live_threads);
      (* Due to reservations being released, we could have [num_live_threads >
         num_reserved_threads]. *)
      assert (t.num_live_threads <= t.max_num_live_threads);
      assert (t.num_reserved_threads <= t.max_num_live_threads);
      File_descr_watcher.invariant t.file_descr_watcher;
      let module Table = Bounded_int_table in
      Table.iter t.fd_by_descr ~f:(fun ~key:file_descr ~data:fd ->
        assert (file_descr = Fd.file_descr fd);
        Fd.invariant fd);
      File_descr_watcher.iter t.file_descr_watcher ~f:(fun read_or_write state fd ->
        try
          match Table.find t.fd_by_descr (Fd.file_descr fd) with
          | None -> failwith "missing from fd_by_descr"
          | Some fd' ->
            if not (Fd.equal fd fd') then fail "fd not equal to" fd' <:sexp_of< Fd.t >>;
            if Fd.num_active_syscalls fd = 0 then
              failwith "zero active syscalls for fd being watched";
            let am_watching =
              let module S = File_descr_watcher.State in
              match state with
              | S.Watching -> true
              | S.Stop_requested -> false
            in
            let fd_is_ready_to = is_some (Fd.ready_to fd read_or_write) in
            if am_watching <> fd_is_ready_to then failwith "ready_to mismatch";
        with exn ->
          fail "fd problem" (exn, read_or_write, fd)
          <:sexp_of< exn * Read_write.Key.t * Fd.t >>);
    with exn -> fail "invariant failed" (exn, t) <:sexp_of< exn * t >>
  end
;;

let create () =
  let num_file_descrs = 1024 in
  let fd_by_descr =
    Bounded_int_table.create ~num_keys:num_file_descrs ~key_to_int:File_descr.to_int
  in
  let (interrupt_select_pipe_read, interrupt_select_pipe_write) = Unix.pipe () in
  let interrupt_select_pipe_read =
    create_fd fd_by_descr Fd.Kind.Fifo interrupt_select_pipe_read
      ~name:"interrupt_select_pipe_read"
  in
  let interrupt_select_pipe_write =
    create_fd fd_by_descr Fd.Kind.Fifo interrupt_select_pipe_write
      ~name:"interrupt_select_pipe_write"
  in
  let finalizer_jobs = Thread_safe_queue.create () in
  let t =
    { mutex = Nano_mutex.create ();

      go_has_been_called = false;

      file_descr_watcher = File_descr_watcher.create ~num_file_descrs;
      fd_by_descr;

      am_in_select_do_cycle = false;
      already_interrupted_select = false;
      interrupt_select_pipe =
        Read_write.create ~read:interrupt_select_pipe_read
          ~write:interrupt_select_pipe_write;

      signals = Tail.create ();
      signal_was_delivered = false;
      delivered_signals = Signal.Table.create ();

      finalizer_jobs;

      num_blocked_threads = 0;
      num_live_threads = 0;
      num_reserved_threads = 0;
      max_num_live_threads = 50;
      work_for_threads = Squeue.create 10;
    }
  in
  t
;;

let signals t = Tail.collect t.signals

(* All of the functions below assume that the scheduler lock is held, except for
   [schedule_from_thread]. *)

let finish_cycle t =
  Core_scheduler.finish_cycle ~now:(Time.now ());
  if not t.am_in_select_do_cycle then
    (* If we are not in the select loop, wake it up so it can process any remaining jobs,
       clock events, or an unhandled exception. *)
    thread_safe_interrupt_select t
;;

let have_lock_do_cycle t =
  Core_scheduler.start_cycle ~now:(Time.now ());
  finish_cycle t;
;;

(* [schedule_from_thread] is run from other threads and so must acquire the lock. *)
let schedule_from_thread t ~within run : unit =
  with_lock t (fun () ->
    Core_scheduler.add_job within run;
    have_lock_do_cycle t)
;;

let create_thread t ?(default_thread_name_first16 = "helper-thread") squeue =
  t.num_live_threads <- t.num_live_threads + 1;
  let dead () = t.num_live_threads <- t.num_live_threads - 1 in
  let (_ : Thread.t) = Thread.create (fun () ->
    let last_thread_name = ref "" in
    let set_thread_name thread_name =
      if String.(<>) thread_name !last_thread_name then begin
        Linux_ext.pr_set_name_first16 thread_name;
        last_thread_name := thread_name;
      end;
    in
    set_thread_name default_thread_name_first16;
    let rec loop () =
      let work = Squeue.pop squeue in
      let thread_name =
        Option.value work.Work.set_thread_name_to
          ~default:default_thread_name_first16
      in
      set_thread_name thread_name;
      match (try work.Work.doit () with e -> dead (); raise e) with
      | `Stop -> dead ()
      | `Continue -> loop ()
    in
    loop ()) ()
  in
  ()
;;

let empty_interrupt_pipe =
  let clearbuffer = String.make 1024 ' ' in
  fun t ->
    if debug then Debug.print "reading from interrupt_select_pipe";
    Fd.syscall_exn (Read_write.get t.interrupt_select_pipe `Read) ~nonblocking:true
      (fun file_descr ->
        let rec loop () =
          let module U = Unix in
          let read_again =
            try
              ignore (U.read_assume_fd_is_nonblocking file_descr
                        clearbuffer ~pos:0
                        ~len:(String.length clearbuffer) : int);
              true
            with
            | U.Unix_error ((U.EWOULDBLOCK | U.EAGAIN), _, _) -> false
          in
          if read_again then loop ()
        in
        loop ())
;;

let request_start_watching t fd read_or_write ~interrupt_select =
  if is_some (Fd.ready_to fd read_or_write) then
    `Already_watching
  else begin
    match Fd.inc_num_active_syscalls fd with
    | `Already_closed -> `Already_closed
    | `Ok ->
      Or_error.ok_exn
        (File_descr_watcher.request_start_watching t.file_descr_watcher fd.Fd.file_descr
           fd read_or_write);
      if interrupt_select then thread_safe_interrupt_select t;
      let ivar = Ivar.create () in
      Fd.set_ready_to fd read_or_write (Some ivar);
      `Watching ivar
  end
;;

let fill_ready_to fd read_or_write value =
  match Fd.ready_to fd read_or_write with
  | None ->
    (* When we fill based on the file-descr watcher, we may have been beaten to the
       punch by a previous request to stop watching, e.g. due to a [Fd.close] or a
       [ready_to_interruptible] that was interrupted.  So, it's OK if the [ready_to] has
       already been cleared. *)
    ()
  | Some ivar ->
    Ivar.fill ivar value;
    Fd.set_ready_to fd read_or_write None;
;;

let request_stop_watching t fd read_or_write value =
  (* We're going to maintain the invariant that the [ready_to] lines up with the
     file-descr watcher's notion of whether the fd is being watched. *)
  if is_some (Fd.ready_to fd read_or_write) then begin
    fill_ready_to fd read_or_write value;
    (* [ready_to] was [Some], so by the invariant we must be watching the file_descr. *)
    Or_error.ok_exn
      (File_descr_watcher.request_stop_watching t.file_descr_watcher
         fd.Fd.file_descr read_or_write);
    (* We interrupt the select loop so that it can process the request.  This is
       necessary even if [t.am_in_select_do_cycle], because the select loop only
       processes requests to stop watching after a call to select.  And we would like
       the existence of such a request to be sufficient to force select to not delay. *)
    thread_safe_interrupt_select t;
  end;
;;

let select_loop t =
  let rec handle_finalizers () =
    if debug then Debug.print "scheduling finalizers";
    match Thread_safe_queue.dequeue t.finalizer_jobs with
    | None -> ()
    | Some (execution_context, work) ->
      Core_scheduler.add_job execution_context work;
      handle_finalizers ()
  in
  let handle_recorded_signals () =
    if debug then Debug.print "handling recorded signals";
    if t.signal_was_delivered then begin
      t.signal_was_delivered <- false;
      Hashtbl.iter t.delivered_signals
        ~f:(fun ~key:signal ~data:r ->
          if !r then begin
            r := false;
            Tail.extend t.signals signal;
          end)
    end
  in
  let compute_timeout () =
    if debug then Debug.print "compute_timeout";
    (* We want to set the timeout to `Zero if there are still jobs remaining, so that we
       immediately come back and start running them after select() checks for I/O. *)
    if Core_scheduler.jobs_left () then
      `Zero
    else begin
      let now = Time.now () in
      match Core_scheduler.next_upcoming_event () with
      | None -> `Forever
      | Some time ->
        if Time.(>) time now then
          `Wait_for (Time.diff time now)
        else
          `Zero
    end
  in
  (* The select loop has the following structure to ensure that interrupt_select's writes
     to the pipe are not dropped.

     loop ()
       do_cycle
       unlock
       select
       lock
       empty_interrupt_pipe
       t.already_interrupted_select <- false;
       handle_finalizers
       loop ()
  *)
  let rec loop () =
    (* At this point, we have the lock. *)
    t.am_in_select_do_cycle <- true; (* so that interrupt_select is a no-op *)
    invariant t;
    have_lock_do_cycle t;
    invariant t;
    t.am_in_select_do_cycle <- false;
    match Core_scheduler.uncaught_exception () with
    | Some exn ->
      unlock t ~allow_from_any_thread:!!true;
      exn
    | None ->
      ignore (request_start_watching t
                (Read_write.get t.interrupt_select_pipe `Read )
                `Read
                ~interrupt_select:false);
      let rec select_loop tries =
        let timeout = compute_timeout () in
        let select_timeout =
          match timeout with
          | `Forever -> (-1.0)
          | `Zero -> 0.0
          | `Wait_for span ->
            (* Wake up at least every second.  Avoid any weirdness due to feeding large
               timeouts to select. *)
            Float.min (Time.Span.to_sec span) 1.0
        in
        if debug then Debug.print "selecting for %g" select_timeout;
        let pre = File_descr_watcher.pre_check t.file_descr_watcher in
        unlock t ~allow_from_any_thread:!!true;
        let check_result =
          File_descr_watcher.thread_safe_check
            t.file_descr_watcher pre ~timeout:select_timeout;
        in
        lock t;
        match File_descr_watcher.post_check t.file_descr_watcher check_result with
        | `Retry ->
          if tries >= 1000 then
            raise (Bug "Could not select, got EINTR 1000 times in a row")
          else begin
            match timeout with
            | `Zero -> select_loop (tries + 1)
            | `Forever | `Wait_for _ -> select_loop 0
          end
        | `Ok post -> post
      in
      let post = select_loop 0 in
      if debug then Debug.print "select returned";
      empty_interrupt_pipe t;
      if debug then Debug.print "done with empty_interrupt_pipe";
      t.already_interrupted_select <- false;
      handle_finalizers ();
      handle_recorded_signals ();
      let handle_post read_or_write =
        let post = Read_write.get post read_or_write in
        let fill fds value =
          List.iter fds ~f:(fun fd ->
            fill_ready_to fd read_or_write value;
            dec_num_active_syscalls_fd t fd)
        in
        let module P = File_descr_watcher in
        fill post.P.bad `Bad_fd;
        fill post.P.ready `Ready;
        List.iter post.P.no_longer_watching ~f:(fun fd -> dec_num_active_syscalls_fd t fd);
      in
      (* We handle writes before reads so that we get all the writes started going to the
         external world before we process all the reads.  This will nicely batch together
         all the output based on the reads for the next writes. *)
      handle_post `Write;
      handle_post `Read;
      loop ();
  in
  let exn =
    try `User_uncaught (loop ())
    with exn -> `Async_uncaught exn
  in
  match exn with
  | `Async_uncaught exn ->
    (* This is a really bad death and causes the program to exit. *)
    fail "select_loop bug" (exn, t) <:sexp_of< exn * t >>
  | `User_uncaught exn ->
    (* Do not put a print statement here.  Having the scheduler raise an
       uncaught exception is the necessary behavior for programs that call
       Scheduler.go and want to handle it. *)
    raise exn
;;

let handle_signal t signal =
  if not (Hashtbl.mem t.delivered_signals signal) then begin
    let r = ref false in
    Hashtbl.replace t.delivered_signals ~key:signal ~data:r;
    Signal.handle signal (fun _ ->
      (* BEGIN UNLOCKED *)
      (* This code is running in a signal handler thread, and so doesn't have the lock.
         We do the [r := true] before the [signal_was_delivered <- true] in case the
         select loop is processing signals concurrently.  If [handle_recorded_signals]
         overwrites our assignment of [signal_was_delivered], then it will see our
         assignment to [r]. *)
      r := true;
      t.signal_was_delivered <- true;
      thread_safe_interrupt_select t;
      (* END UNLOCKED *)
    );
  end
;;

let finalize t f obj =
  let e = Core_scheduler.current_execution_context () in
  (* We use [Caml.Gc.finalise] instead of [Core.Std.Gc.finalise] because the latter has
     its own wrapper around [Caml.Gc.finalise] to run finalizers synchronously. *)
  Caml.Gc.finalise (fun x ->
    Thread_safe_queue.enqueue t.finalizer_jobs (e, (fun () -> f x));
    thread_safe_interrupt_select t)
    obj;
;;

(* [go] is at the core of every program that uses Async.  If there is a bug in the code
   below, e.g. an unhandled exception, then it will cause the Async program to die.  The
   monitor system will have no chance.  Thus, the code that implements [go] should be read
   especially carefully.  [go] is called from the main thread and so must acquire the lock
   if the thread has not already done so implicitly via use of an async operation that
   used [the_one_and_only]. *)
let go t () =
  if debug then Debug.print "Scheduler.go";
  if not (am_in_async t) then Nano_mutex.lock_exn t.mutex;
  if t.go_has_been_called then begin
    (* Someone else has run the scheduler already, so we are just going
       to never return. *)
    Nano_mutex.unlock_exn t.mutex;
    thread_safe_interrupt_select t;
    Time.pause_forever ()
  end else begin
    (* We handle [Signal.pipe]  so that write() calls on a closed pipe/socket get EPIPE but
      the process doesn't die due to an unhandled SIGPIPE. *)
    handle_signal t Signal.pipe;
    t.go_has_been_called <- true;
    select_loop t;
  end
;;

(* When a block group becomes unreachable, use a finalizer to free up its
   reserved threads. *)
let finalize_block_group t bg =
  t.num_reserved_threads <-
    t.num_reserved_threads - bg.Block_group.num_reserved_threads;
;;

let create_block_group t ?(min_reserved_threads = 0)
    ?(max_reserved_threads = max_int) () =
  let reserved = t.num_reserved_threads + min_reserved_threads in
  if reserved > t.max_num_live_threads then
    `Out_of_threads
  else begin
    t.num_reserved_threads <- reserved;
    let bg = Block_group.create ~min_reserved_threads ~max_reserved_threads in
    Block_group.invariant bg;
    finalize t (fun bg -> finalize_block_group t bg) bg;
    `Ok bg;
  end
;;

let init () =
  the_one_and_only_ref :=
    Ready_to_initialize
      (fun () ->
        let t = create () in
        let main_block_group =
          match create_block_group t ~min_reserved_threads:1 () with
          | `Ok t -> t
          | `Out_of_threads -> failwith "Async unable to create main block group"
        in
        Core_scheduler.set_block_group main_block_group;
        Core_scheduler.set_main_execution_context (Core_scheduler.current_execution_context ());
        t)

(* This is a top-level effect, but it simply fills in a ref cell with a closure,
   and is therefore OK. *)
let () = init ()

let delta_reserved_threads t bg i =
  if debug then Debug.print "delta_reserved_threads %d" i;
  t.num_reserved_threads <- t.num_reserved_threads + i;
  bg.Block_group.num_reserved_threads <- bg.Block_group.num_reserved_threads + i;
;;

let delta_blocked_threads t bg i =
  t.num_blocked_threads <- t.num_blocked_threads + i;
  bg.Block_group.num_blocked_threads <- bg.Block_group.num_blocked_threads + i;
;;

let get_thread t bg =
  if bg.Block_group.num_blocked_threads < bg.Block_group.num_reserved_threads then begin
    delta_blocked_threads t bg 1;
    `Ok
  end else if (t.num_reserved_threads < t.max_num_live_threads
                && (bg.Block_group.num_reserved_threads
                    < bg.Block_group.max_reserved_threads)) then begin
    delta_reserved_threads t bg 1;
    delta_blocked_threads t bg 1;
    `Ok;
  end else
      `Out_of_threads
;;

let finish_with_thread t bg =
  delta_blocked_threads t bg (-1);
  (* If we don't need to reserve this thread, then unreserve it. *)
  if bg.Block_group.num_reserved_threads > bg.Block_group.min_reserved_threads
  then delta_reserved_threads t bg (-1);
;;

module Helper_thread = struct
  type t = {
    work_queue : Work.t Squeue.t;
  }
end

let create_helper_thread ?name_first16 t =
  let execution_context = Core_scheduler.current_execution_context () in
  let block_group = execution_context.Execution_context.block_group in
  if t.num_live_threads = t.max_num_live_threads then
    `Out_of_threads
  else
    match get_thread t block_group with
    | `Out_of_threads as x -> x
    | `Ok ->
        let work_queue = Squeue.create 10 in
        create_thread t work_queue ?default_thread_name_first16:name_first16;
        let finished () =
          finish_with_thread t block_group;
          Squeue.push_uncond work_queue
            { Work.
              set_thread_name_to = None;
              doit = (fun () -> `Stop);
            }
        in
        let helper_thread = { Helper_thread. work_queue } in
        finalize t (fun _ -> finished ()) helper_thread;
        `Ok helper_thread;
;;

let thread_safe_pipe ?max_batch ?(allow_straight_through = true) t =
  let am_closed = ref false in
  let helper_thread_should_run = ref true in
  let squeue = Squeue.create 100 in
  let queue = Queue.create () in
  let pipe_reader, pipe_writer = Pipe.create () in
  let extend x = ignore (Pipe.write pipe_writer x) in
  let have_lock_do_process () =
    if not (Queue.is_empty queue) then begin
      let close () =
        helper_thread_should_run := false;
        Squeue.push_uncond squeue `Closed (* to wake the helper thread *)
      in
      begin match max_batch with
      | None ->
        Queue.iter queue ~f:(function
          | `Closed -> close ()
          | `Extend x -> extend x);
        Queue.clear queue;
        have_lock_do_cycle t
      | Some max_batch ->
        let deque_many t limit =
          let rec loop i batch =
            if i < limit then begin
              match Queue.dequeue t with
              | None -> List.rev batch
              | Some x -> loop (i + 1) (x :: batch)
            end else
              List.rev batch
          in
          loop 0 []
        in
        let rec loop () =
          match deque_many queue max_batch with
          | [] -> ()
          | batch ->
            List.iter batch ~f:(function
              | `Closed -> close ()
              | `Extend x -> extend x);
            have_lock_do_cycle t;
            loop ()
        in
        loop ()
      end
    end
  in
  let process () =
    Core.Std.protect
      ~f:have_lock_do_process
      ~finally:(fun () -> unlock t)
  in
  let (_ : Thread.t) =
    Thread.create (fun () ->
      Linux_ext.pr_set_name_first16 "thr-safe-stream";
      while !helper_thread_should_run do
        Squeue.wait_not_empty squeue;
        lock t;
        Squeue.transfer_queue_nowait squeue queue;
        process ()
      done)
      ()
  in
  let put x =
    if !am_closed then failwith "put to pipe that has been closed";
    if am_in_async t then failwith "In_thread.pipe [put] called from within Async";
    if allow_straight_through && try_lock t then begin
      Squeue.transfer_queue_nowait squeue queue;
      Queue.enqueue queue (`Extend x);
      process ()
    end else
      Squeue.push_uncond squeue (`Extend x)
  in
  let close () =
    if am_in_async t then failwith "In_thread.pipe [close] called from within Async";
    if not !am_closed then begin
      am_closed := true;
      Squeue.push_uncond squeue `Closed
    end
  in
  (pipe_reader, put, close)
;;

let thread_safe_deferred t =
  let i = Ivar.create () in
  let put x =
    with_lock t (fun () ->
      Ivar.fill i x;
      have_lock_do_cycle t)
  in
  (Ivar.read i, put)
;;

let run_in_thread t ?thread ?name_first16 f =
  let execution_context = Core_scheduler.current_execution_context () in
  let block_group = execution_context.Execution_context.block_group in
  Deferred.create (fun ivar ->
    let add_work_for_threads w = Squeue.push_uncond t.work_for_threads w in
    let doit () =
      let result = Result.try_with f in
      schedule_from_thread t ~within:execution_context (fun () ->
        begin match thread with
        | Some _ -> ()
        | None ->
          match Queue.dequeue block_group.Block_group.work with
          | Some w -> add_work_for_threads w
          | None -> finish_with_thread t block_group
        end;
        Ivar.fill ivar (Result.raise_error result));
      `Continue;
    in
    let work = { Work. doit; set_thread_name_to = name_first16 } in
    match thread with
    | Some helper_thread ->
      Squeue.push_uncond helper_thread.Helper_thread.work_queue work
    | None ->
      match get_thread t block_group with
      | `Out_of_threads ->
        Queue.enqueue block_group.Block_group.work work
      | `Ok ->
        if t.num_blocked_threads > t.num_live_threads then
          create_thread t t.work_for_threads;
        add_work_for_threads work)
;;

let run_cycle (type a) (type b)
    (f : unit -> a)
    ~(after_cycle_end : (a, exn) Result.t -> b) : b =
  let t = the_one_and_only ~should_lock:false () in
  if not (am_in_async t) then Nano_mutex.lock_exn t.mutex;
  Core_scheduler.start_cycle ~now:(Time.now ());
  (* We run [f] within the [main_execution_context] so that any errors are sent to its
     monitor, rather than whatever random monitor happened to be in effect. *)
  let res =
    Result.try_with (fun () ->
      Core_scheduler.with_execution_context (Core_scheduler.main_execution_context ()) ~f)
  in
  finish_cycle t;
  let res = after_cycle_end res in
  Nano_mutex.unlock_exn t.mutex;
  res
;;

let run_in_async f = run_cycle f ~after_cycle_end:Fn.id

let go ?raise_unhandled_exn () =
  let doit () = go (the_one_and_only ()) () in
  match raise_unhandled_exn with
  | Some () -> doit ()
  | None ->
    Exn.handle_uncaught ~exit:true (fun () -> never_returns (doit ()));
    assert false
;;

let go_main ?raise_unhandled_exn ~main () =
  begin match !the_one_and_only_ref with
  | Ready_to_initialize _ -> ()
  | Not_ready_to_initialize -> assert false
  | Initialized _ -> failwith "Async initialized prior to go_main"
  end;
  Deferred.upon Deferred.unit main;
  go ?raise_unhandled_exn ();
;;

let go_in_thread () = ignore (Thread.create (fun () -> never_returns (go ())) ())

let block_on_async (type a) (f : unit -> a Deferred.t) =
  let t = the_one_and_only ~should_lock:false () in
  (* No need to create a thread if the scheduler isn't already running. *)
  if not t.go_has_been_called then
    ignore (Thread.create
              (fun () ->
                Exn.handle_uncaught ~exit:true (fun () -> never_returns (go ())))
              ());
  (* After we get the value we're waiting for, we give the lock back to the calling
     thread.  Scheduled async jobs will block until he starts the scheduler again. *)
  let maybe_blocked =
    run_cycle
      (fun () -> Monitor.try_with f ~name:"block_on_async")
      ~after_cycle_end:(fun res ->
        match res with
        | Ok d -> begin
          match Deferred.peek d with
          | Some v -> `Available v
          | None ->
            let q = Squeue.create 1 in
            upon d (fun v -> Squeue.push_uncond q v);
            (* Squeue.pop can block, so we have to do it outside async *)
            `Blocked_wait_on_squeue q
          end
        | Error exn -> `Available (Error exn))
  in
  let res =
    match maybe_blocked with
    | `Available v -> v
    | `Blocked_wait_on_squeue q -> Squeue.pop q
  in
  if (is_main_thread ()) then Nano_mutex.lock_exn t.mutex;
  res
;;

let block_on_async_exn f = Result.raise_error (block_on_async f)

exception Called_run_in_async_wait_from_main_thread with sexp
exception Called_run_in_async_wait_from_async with sexp
exception Returning_from_run_in_async_wait_without_releasing_the_lock with sexp

let run_in_async_wait f =
  if (is_main_thread ())
  then raise Called_run_in_async_wait_from_main_thread;

  let t = the_one_and_only ~should_lock:false () in
  if (am_in_async t)
  then raise Called_run_in_async_wait_from_async;

  let res = block_on_async f in

  (* We expect this case to never happen, but we check
     to make sure things haven't gone terribly wrong. *)
  if (am_in_async t)
  then raise Returning_from_run_in_async_wait_without_releasing_the_lock
  else res
;;

let run_in_async_wait_exn f = Result.raise_error (run_in_async_wait f)

let is_ready_to_initialize () =
  match !the_one_and_only_ref with
  | Not_ready_to_initialize -> false
  | Initialized _ -> false
  | Ready_to_initialize _ -> true
;;

type 'a with_options = 'a Core_scheduler.with_options

include struct
  open Core_scheduler

  let current_execution_context = current_execution_context
  let cycle_count = cycle_count
  let cycle_start = cycle_start
  let cycle_times = cycle_times
  let num_pending_jobs = num_pending_jobs
  let schedule = schedule
  let schedule' = schedule'
  let set_max_num_jobs_per_priority_per_cycle = set_max_num_jobs_per_priority_per_cycle
  let within = within
  let within' = within'
  let within_context = within_context
  let within_v = within_v
end

let is_running () = (the_one_and_only ()).go_has_been_called

let finalize f x = finalize (the_one_and_only ()) f x

let report_long_cycle_times ?(cutoff = sec 1.) () =
  Stream.iter (cycle_times ())
    ~f:(fun span ->
      if Time.Span.(>) span cutoff then
        eprintf "%s\n%!"
          (Error.to_string_hum
             (Error.create "long async cycle" span <:sexp_of< Time.Span.t >>)))
;;


