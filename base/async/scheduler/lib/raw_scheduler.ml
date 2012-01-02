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
    file_descr_watcher : (Fd.t, Fd.ready_to_result Ivar.t) File_descr_watcher.t;
    (* [fd_by_descr] holds every file descriptor that Async knows about.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : Fd_by_descr.t;
    mutable id_of_thread_running_the_select_loop : int;
    (* The [select_interruptor] is used to interrupt the call to select when the select
       loop needs to be woken up to process changes, for any of the following reasons:

       * to start watching a file descriptor
       * to add a timer event
       * to process a finalizer
       * to process a signal
       * to process a toplevel unhandled exception *)
    select_interruptor : Interruptor.t;

    signal_handlers : Raw_signal_handlers.t;

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

let am_in_async t = Nano_mutex.current_thread_has_lock t.mutex

let current_thread_id () = Thread.id (Thread.self ())

let is_main_thread () = current_thread_id () = 0

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
      File_descr_watcher.iter t.file_descr_watcher ~f:(fun read_or_write _state fd ->
        try
          match Table.find t.fd_by_descr (Fd.file_descr fd) with
          | None -> failwith "missing from fd_by_descr"
          | Some fd' ->
            if not (Fd.equal fd fd') then fail "fd not equal to" fd' <:sexp_of< Fd.t >>;
            if Fd.num_active_syscalls fd = 0 then
              failwith "zero active syscalls for fd being watched";
        with exn ->
          fail "fd problem" (exn, read_or_write, fd)
          <:sexp_of< exn * Read_write.Key.t * Fd.t >>);
    with exn -> fail "invariant failed" (exn, t) <:sexp_of< exn * t >>
  end
;;

let create () =
  let num_file_descrs = 1024 in
  let fd_by_descr =
    Bounded_int_table.create
      ~num_keys:num_file_descrs
      ~key_to_int:File_descr.to_int
      ~sexp_of_key:File_descr.sexp_of_t
      ()
  in
  let finalizer_jobs = Thread_safe_queue.create () in
  let select_interruptor = Interruptor.create fd_by_descr in
  let signal_handlers =
    Raw_signal_handlers.create
      ~thread_safe_notify_signal_delivered:(fun () ->
        Interruptor.thread_safe_interrupt select_interruptor)
  in
  let t =
    { mutex = Nano_mutex.create ();
      go_has_been_called = false;
      file_descr_watcher = File_descr_watcher.create ~num_file_descrs;
      fd_by_descr;
      id_of_thread_running_the_select_loop = -1; (* set when [select_loop] is called *)
      select_interruptor;
      signal_handlers;
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

let thread_safe_interrupt_select t =
  Interruptor.thread_safe_interrupt t.select_interruptor
;;

(* All of the functions below assume that the scheduler lock is held, except for
   [schedule_from_thread]. *)

let finish_cycle t =
  Core_scheduler.finish_cycle ~now:(Time.now ());
  if current_thread_id () <> t.id_of_thread_running_the_select_loop then
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

let request_start_watching t fd read_or_write ~interrupt_select =
  match Fd.inc_num_active_syscalls fd with
  | `Already_closed -> `Already_closed
  | `Ok ->
    let ivar = Ivar.create () in
    match
      File_descr_watcher.request_start_watching t.file_descr_watcher fd.Fd.file_descr
        fd ivar read_or_write
    with
    | `Already_watching ->
      dec_num_active_syscalls_fd t fd;
      `Already_watching
    | `Ok ->
      if interrupt_select then thread_safe_interrupt_select t;
      `Watching ivar
;;

let request_stop_watching t fd read_or_write value =
  match
    File_descr_watcher.request_stop_watching t.file_descr_watcher
      fd.Fd.file_descr read_or_write
  with
  | `Was_not_watching -> ()
  | `Ok ready_to ->
    Ivar.fill ready_to value;
    (* We interrupt the select loop so that it can process the request.  This is
       necessary even if [t.am_in_select_do_cycle], because the select loop only
       processes requests to stop watching after a call to select.  And we would like
       the existence of such a request to be sufficient to force select to not delay. *)
    thread_safe_interrupt_select t;
;;

let select_loop t =
  t.id_of_thread_running_the_select_loop <- current_thread_id ();
  let rec handle_finalizers () =
    if debug then Debug.print "scheduling finalizers";
    match Thread_safe_queue.dequeue t.finalizer_jobs with
    | None -> ()
    | Some (execution_context, work) ->
      Core_scheduler.add_job execution_context work;
      handle_finalizers ()
  in
  let handle_delivered_signals () =
    if debug then Debug.print "handling delivered signals";
    Raw_signal_handlers.handle_delivered t.signal_handlers;
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
       Interruptor.clear
       t.already_interrupted_select <- false;
       handle_finalizers
       loop ()
  *)
  let rec loop () =
    (* At this point, we have the lock. *)
    invariant t;
    have_lock_do_cycle t;
    invariant t;
    match Core_scheduler.uncaught_exception () with
    | Some exn ->
      unlock t ~allow_from_any_thread:!!true;
      exn
    | None ->
      begin
        match
          request_start_watching t (Interruptor.read_fd t.select_interruptor) `Read
            ~interrupt_select:false
        with
      | `Already_watching | `Watching _ -> ()
      | `Already_closed -> fail "can not watch select interruptor" t <:sexp_of< t >>
      end;
      let rec select_loop () =
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
        | `Retry -> select_loop ()
        | `Ok post -> post
      in
      let post = select_loop () in
      if debug then Debug.print "select returned";
      Interruptor.clear t.select_interruptor;
      if debug then Debug.print "done with empty_interrupt_pipe";
      handle_finalizers ();
      handle_delivered_signals ();
      let handle_post read_or_write =
        let post = Read_write.get post read_or_write in
        let module P = File_descr_watcher in
        let fill zs value =
          List.iter zs ~f:(fun (fd, ready_to) ->
            Ivar.fill ready_to value;
            dec_num_active_syscalls_fd t fd);
        in
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
    (* Someone else has run the scheduler already, so we are just going to never
       return. *)
    Nano_mutex.unlock_exn t.mutex;
    thread_safe_interrupt_select t;
    Time.pause_forever ()
  end else begin
    (* We handle [Signal.pipe] so that write() calls on a closed pipe/socket get EPIPE but
       the process doesn't die due to an unhandled SIGPIPE. *)
    Raw_signal_handlers.handle_signal t.signal_handlers Signal.pipe;
    t.go_has_been_called <- true;
    select_loop t;
  end
;;

(* When a block group becomes unreachable, use a finalizer to free up its reserved
   threads. *)
let finalize_block_group t bg =
  t.num_reserved_threads <- t.num_reserved_threads - bg.Block_group.num_reserved_threads;
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
      Core_scheduler.set_main_execution_context
        (Core_scheduler.current_execution_context ());
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
        Ivar.fill ivar (Result.ok_exn result));
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

let run_in_async_gen (type a) (type b)
    (f : unit -> [ `Do_not_run_a_cycle | `Run_a_cycle ] * a)
    (after_cycle_end : (a -> b)) : b =
  let t = the_one_and_only ~should_lock:false () in
  if not (am_in_async t) then Nano_mutex.lock_exn t.mutex;
  (* We run [f] within the [main_execution_context] so that any errors are sent to its
     monitor, rather than whatever random monitor happened to be in effect. *)
  Core.Std.protect ~finally:(fun () -> Nano_mutex.unlock_exn t.mutex) ~f:(fun () ->
    let maybe_run_a_cycle, res =
      Core_scheduler.with_execution_context ~f
        (Core_scheduler.main_execution_context ())
    in
    begin match maybe_run_a_cycle with
    | `Do_not_run_a_cycle -> ()
    | `Run_a_cycle -> have_lock_do_cycle t
    end;
    after_cycle_end res)
;;

let run_cycle (type a) (type b)
    (f : unit -> a)
    ~(after_cycle_end : (a, exn) Result.t -> b) : b =
  run_in_async_gen (fun () -> `Run_a_cycle, Result.try_with f) after_cycle_end
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

exception Called_block_on_async_from_async with sexp;;

let block_on_async (type a) (f : unit -> a Deferred.t) =
  let t = the_one_and_only ~should_lock:false () in
  if (am_in_async t)
  then raise Called_block_on_async_from_async;

  (* Only create a scheduler thread if the scheduler isn't already running. *)
  if not t.go_has_been_called then
    ignore (Thread.create
              (fun () ->
                Exn.handle_uncaught ~exit:true (fun () -> never_returns (go ())))
              ());
  (* If this was called from the main thread, then after we get the value we're waiting
     for, we give the lock back.  Scheduled async jobs will block until he starts the
     scheduler again. *)
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
  if is_main_thread () then Nano_mutex.lock_exn t.mutex;
  res
;;

let block_on_async_exn f = Result.ok_exn (block_on_async f)

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

let run_in_async_wait_exn f = Result.ok_exn (run_in_async_wait f)

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
