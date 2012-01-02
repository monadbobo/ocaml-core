open Core.Std
open Import
open Deferred_std
module Stream = Async_stream
module Q = Queue

let show_debug_messages = ref false
let check_invariant = ref false

let message tag sexp_of_a a =
  Core.Std.eprintf "%s: %s\n%!" tag (Sexp.to_string_hum (sexp_of_a a))
;;

module Read = struct
  (* [Read.t] represents a blocked read attempt.  If someone reads from an empty pipe,
     they enqueue a [Read.t] in the queue of [blocked_reads].  Later, when values are
     written to a pipe, that will cause some number of blocked reads to be filled, first
     come first serve.  [how_many] specifies how many values a read should consume from
     the pipe when it gets its turn.  [All] means to consume everything.
     [At_most num_values] means to consume from 0 to num_values values.

     If a pipe is closed, then all blocked reads will be filled with `Eof. *)
  type how_many =
  | All
  | At_most of int
  with sexp_of

  type 'a t =
    { how_many : how_many;
      ready : [ `Eof | `Ok of 'a Q.t ] Ivar.t;
    }
  with sexp_of

  let invariant (type a) t =
    try
      match t.how_many with
      | All -> ()
      | At_most i -> assert (i >= 0);
    with
    | exn -> fail "invariant failed" (exn, t) <:sexp_of< exn * a t >>
  ;;

  let is_empty t = Ivar.is_empty t.ready

  let fill t x = Ivar.fill t.ready x
end

module Flush = struct
  (* A [Flush.t] represents a blocked flush operation, which can be enabled by a future
     read.  If someone does a [flushed p] on a pipe, that blocks until everything that's
     currently in the pipe at that point has drained out of the pipe.  When we call
     [flushed], it records the total amount of data that has been written so far in
     [amount_written_when_enqueued].  We can fill the [Flush.t] with [`Ok] when this amount
     of data has been read from the pipe.

     A [Flush.t] can also be filled with [`Reader_closed], which happens when the
     reader end of the pipe is closed, and we are thus sure that the unread relements
     preceding the flush will never be read. *)
  type t =
    { fill_when_amount_read : int;
      ready : [ `Ok | `Reader_closed ] Ivar.t;
    }
  with fields, sexp_of

  let fill t v = Ivar.fill t.ready v
end

type ('a, 'phantom) t =
  { (* [id] is an integer used to distinguish pipes when debugging. *)
    id : int;
    (* [buffer] holds values written to the pipe that have not yet been read. *)
    buffer : 'a Q.t;
    (* [size_budget] governs pushback on writers to the pipe.

       There is *no* invariant that [amount_written - amount_read <= size_budget].  There
       is no hard upper bound on the number of elements that can be stuffed into the
       [buffer].  This is due to the way we handle writes.  When we do a write, all of the
       values written are immediately enqueued into [buffer].  After the write, if [length
       t <= t.size_budget], then the writer will be notified to continue writing.  After
       the write, if [length t > t.size_budget], then the write will block until the pipe
       is under budget.

       [pushback] is full iff [length t <= t.size_budget || is_closed t] *)
    mutable size_budget : int;
    mutable pushback : unit Ivar.t;
    (* [blocked_flushes] holds flushes whose preceding elements hae not been completely
       read.  For each blocked flush, the number of elements that need to be read from the
       pipe in order to fill the flush is:

       fill_when_amount_read - amount_read_for_flushes

       Keeping the data in this form allows us to change a single field when we consume
       items instead of having to iterate over the whole queue of flushes. *)
    mutable amount_read_for_flushes : int;
    blocked_flushes : Flush.t Q.t;
    (* [blocked_reads] holds reads that are waiting on data to be written to the pipe. *)
    blocked_reads : 'a Read.t Q.t;
    (* [closed] is filled when we close the write end of the pipe. *)
    closed : unit Ivar.t;
  }
with fields, sexp_of

let hash t = Hashtbl.hash t.id
let equal t1 t2 = phys_equal t1 t2

type ('a, 'phantom) pipe = ('a, 'phantom) t with sexp_of

let is_empty t = Q.is_empty t.buffer

let is_closed t = Ivar.is_full t.closed

let close_called t = Ivar.read t.closed

let pushback t = Ivar.read t.pushback

let sexp_of_t_ignore t = sexp_of_t (fun _ -> Sexp.Atom "X") (fun _ -> assert false) t

let length t = Q.length t.buffer

let invariant (type a) (type phantom) t =
  try
    assert (t.size_budget >= 0);
    assert (Ivar.is_full t.pushback = (length t <= t.size_budget || is_closed t));
    let module F = Flush in
    if Q.is_empty t.blocked_flushes then
      t.amount_read_for_flushes <- 0;
    Q.iter t.blocked_flushes ~f:(fun f ->
      assert (f.F.fill_when_amount_read > t.amount_read_for_flushes));
    assert (List.is_sorted ~compare
              (List.map (Q.to_list t.blocked_flushes) ~f:F.fill_when_amount_read));
    if is_empty t then begin
      assert (t.amount_read_for_flushes = 0);
      assert (Q.is_empty t.blocked_flushes);
    end;
    (* If data is available, no one is waiting for it. *)
    if not (is_empty t) then assert (Q.is_empty t.blocked_reads);
    Q.iter t.blocked_reads ~f:(fun read ->
      Read.invariant read;
      assert (Read.is_empty read));
    (* You never block trying to read a closed pipe. *)
    if is_closed t then assert (Q.is_empty t.blocked_reads);
  with
  | exn -> fail "invariant failed" (exn, t) <:sexp_of< exn * (a, phantom) t >>
;;

module Reader = struct
  type phantom with sexp_of
  type 'a t = ('a, phantom) pipe with sexp_of
  let invariant = invariant
end

module Writer = struct
  type phantom with sexp_of
  type 'a t = ('a, phantom) pipe with sexp_of
  let invariant = invariant
end

let id_ref = ref 0

let create () =
  incr id_ref;
  let full_ivar () = let i = Ivar.create () in Ivar.fill i (); i in
  let t =
    { id                                  = !id_ref;
      closed                              = Ivar.create ();
      size_budget                         = 0;
      pushback                            = full_ivar ();
      buffer                              = Q.create ();
      amount_read_for_flushes = 0;
      blocked_flushes                     = Q.create ();
      blocked_reads                       = Q.create ();
    }
  in
  if !check_invariant then invariant t;
  (t, t)
;;

let update_pushback t =
  if length t <= t.size_budget then
    Ivar.fill_if_empty t.pushback ()
  else if Ivar.is_full t.pushback then
    t.pushback <- Ivar.create ();
;;

(* [consume_all t] reads all the elements in [t], in constant time. *)
let consume_all t =
  Q.iter t.blocked_flushes ~f:(fun flush -> Flush.fill flush `Ok);
  Q.clear t.blocked_flushes;
  t.amount_read_for_flushes <- 0;
  let result = Q.create () in
  Q.transfer ~src:t.buffer ~dst:result;
  Ivar.fill_if_empty t.pushback ();
  result
;;

let rec fill_flushes t =
  match Q.peek t.blocked_flushes with
  | None -> t.amount_read_for_flushes <- 0
  | Some flush ->
    if t.amount_read_for_flushes >= flush.Flush.fill_when_amount_read then begin
      Flush.fill flush `Ok;
      ignore (Q.dequeue_exn t.blocked_flushes);
      fill_flushes t;
    end
;;

(* [consume_at_most t num_values] reads [min num_values (num_queued t)] items.  It is an
   error if [is_empty t] or [num_values < 0]. *)
let consume_at_most t num_values =
  assert (num_values >= 0);
  if num_values >= length t then
    consume_all t (* fast because it can use [Q.transfer] *)
  else begin
    if Q.length t.blocked_flushes > 0 then begin
      (* No need to check for overflow.  You'd need to write 2^62 elements to the pipe,
         which would take about 146 years, at a flow rate of 1 size-unit/nanosecond. *)
      t.amount_read_for_flushes <- t.amount_read_for_flushes + num_values;
      fill_flushes t;
    end;
    let result = Q.create () in
    for i = 1 to num_values do
      Q.enqueue result (Q.dequeue_exn t.buffer);
    done;
    update_pushback t;
    result
  end
;;

let set_size_budget t size_budget =
  if size_budget <> t.size_budget then begin
    if size_budget < 0 then fail "negative size_budget" size_budget <:sexp_of< int >>;
    t.size_budget <- size_budget;
    update_pushback t;
  end
;;

let start_write t =
  if !show_debug_messages then message "write" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  if is_closed t then fail "write to closed pipe" t sexp_of_t_ignore;
;;

let fill_blocked_reads t =
  while not (Q.is_empty t.blocked_reads) && not (is_empty t) do
    let read = Q.dequeue_exn t.blocked_reads in
    let module R = Read in
    R.fill read
      (`Ok
          (match read.R.how_many with
          | R.All -> consume_all t
          | R.At_most n -> consume_at_most t n));
  done;
;;

let write' t values =
  start_write t;
  Q.transfer ~src:values ~dst:t.buffer;
  fill_blocked_reads t;
  update_pushback t;
  pushback t;
;;

let write_no_pushback t value =
  start_write t;
  Q.enqueue t.buffer value;
  fill_blocked_reads t;
  update_pushback t;
;;

let write t value =
  write_no_pushback t value;
  pushback t;
;;

let with_write t ~f =
  pushback t
  >>| fun () ->
  if is_closed t then
    `Closed
  else
    `Ok (f (fun x -> write_no_pushback t x))
;;

let read_now t =
  if !show_debug_messages then message "read_now" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  if is_empty t then begin
    if is_closed t then `Eof
    else `Nothing_available
  end else begin
    assert (Q.is_empty t.blocked_reads);
    `Ok (consume_all t)
  end
;;

let clear t =
  match read_now t with
  | `Eof | `Nothing_available | `Ok _ -> ()
;;

let enqueue_blocked_read t how_many =
  Deferred.create (fun ready -> Q.enqueue t.blocked_reads { Read. how_many; ready })
;;

let read' t =
  if !show_debug_messages then message "read'" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  match read_now t with
  | (`Ok _ | `Eof) as r -> return r
  | `Nothing_available -> enqueue_blocked_read t Read.All
;;

let read_at_most t ~num_values =
  if !show_debug_messages then message "read_at_most" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  if num_values < 0 then fail "read_at_most num_values < 0" num_values <:sexp_of< int >>;
  if is_empty t then begin
    if is_closed t then
      return `Eof
    else
      enqueue_blocked_read t (Read.At_most num_values)
  end else begin
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_at_most t num_values))
  end
;;

let values_available t =
  read_at_most t ~num_values:0
  >>| function
    | `Eof -> `Eof
    | `Ok q -> assert (Q.is_empty q); `Ok
;;

let read t =
  read_at_most t ~num_values:1
  >>| function
    | `Eof -> `Eof
    | `Ok q -> assert (Q.length q = 1); `Ok (Q.dequeue_exn q)
;;

(* [read_exactly t ~num_values] loops getting you all [num_values] items, unless EOF. *)
let read_exactly t ~num_values =
  if !show_debug_messages then message "read_exactly" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  if num_values <= 0 then
    fail "read_exactly got num_values <= 0" num_values <:sexp_of< int >>;
  Deferred.create (fun finish ->
    let result = Q.create () in
    let rec loop () =
      let already_read = Q.length result in
      if already_read = num_values then
        Ivar.fill finish (`Exactly result)
      else begin
        read_at_most t ~num_values:(num_values - already_read)
        >>> function
          | `Eof -> Ivar.fill finish (if already_read = 0 then `Eof else `Fewer result)
          | `Ok q ->
            Q.transfer ~src:q ~dst:result;
            loop ();
      end
    in
    loop ())
;;

let flushed t =
  if is_empty t then
    return `Ok
  else
    (* [t] might be closed.  But the read end can't be closed, because if it were, then
       [t] would be empty.  If the write end is closed but not the read end, then we want
       to enqueue a blocked flush because the enqueued values may get read. *)
    Deferred.create (fun ready ->
      Q.enqueue t.blocked_flushes
        { Flush.
          fill_when_amount_read = t.amount_read_for_flushes + length t;
          ready;
        })
;;

let close t =
  if !show_debug_messages then message "close" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  if Ivar.is_empty t.closed then begin
    Ivar.fill t.closed ();
    Ivar.fill_if_empty t.pushback ();
    if is_empty t then begin
      Q.iter  t.blocked_reads ~f:(fun read -> Read.fill read `Eof);
      Q.clear t.blocked_reads;
    end;
  end;
;;

let close_read t =
  if !show_debug_messages then message "close_read" sexp_of_t_ignore t;
  if !check_invariant then invariant t;
  clear t;
  close t;
  Q.iter  t.blocked_flushes ~f:(fun flush -> Flush.fill flush `Reader_closed);
  Q.clear t.blocked_flushes;
;;

let fold_gen t ~init ~f =
  if !check_invariant then invariant t;
  Deferred.create (fun finished ->
    let rec loop b =
      read' t >>> function
      | `Eof  -> Ivar.fill finished b
      | `Ok q -> f b q loop
    in
    loop init)
;;

let fold' t ~init ~f = fold_gen t ~init ~f:(fun b q loop -> f b q >>> loop)

let fold t ~init ~f = fold_gen t ~init ~f:(fun init q loop -> loop (Q.fold q ~init ~f))

let iter_gen t ~f = fold_gen t ~init:() ~f:(fun () q loop -> f q loop)

let drain t = iter_gen t ~f:(fun _ loop -> loop ())

let drain_and_count t = fold_gen t ~init:0 ~f:(fun sum q loop -> loop (sum + Q.length q))

let iter' t ~f = fold' t ~init:() ~f:(fun () q -> f q)

let iter t ~f = iter' t ~f:(fun q -> Deferred.Queue.iter q ~f)

let iter_without_pushback t ~f = iter_gen t ~f:(fun q loop -> Q.iter q ~f; loop ())

let to_stream t =
  Stream.create (fun tail ->
    iter_without_pushback t ~f:(fun x -> Tail.extend tail x)
    >>> fun () ->
    Tail.close_exn tail)
;;

(* The implementation of [of_stream] does as much batching as possible.  It grabs as many
   items as are available into an internal queue.  Once it's grabbed everything, it writes
   it to the pipe and then blocks waiting for the next element from the stream.

   There's no possibility that we'll starve the pipe reading an endless stream, just
   accumulating the elements into our private queue forever without ever writing them
   downstream to the pipe.  Why? because while we're running, the stream-producer *isn't*
   running -- there are no Async block points in the queue-accumulator loop.  So the
   queue-accumulator loop will eventually catch up to the current stream tail, at which
   point we'll do the pipe-write and then block on the stream... thus giving the
   stream-producer a chance to make more elements.

   One can't implement [of_stream] using [Stream.iter] or [Stream.iter'] because you
   need to be able to stop early when the consumer closes the pipe.  Also, using either
   of those would entail significantly more deferred overhead, whereas the below
   implementation uses a deferred only when it needs to block on the stream or on the
   pipe's consumer. *)
let of_stream s =
  let r, w = create () in
  let q = Q.create () in
  let transfer () =
    if not (Q.is_empty q) then
      (* Can not pushback on the stream, so ignore the pipe's pushback. *)
      whenever (write' w q);
  in
  let rec loop s =
    if not (is_closed w) then begin
      let next = Stream.next s in
      match Deferred.peek next with
      | Some next -> loop_next next
      | None ->
        transfer ();
        upon next loop_next;
    end
  and loop_next = function
    | Stream.Nil -> transfer (); close w
    | Stream.Cons (x, s) -> Q.enqueue q x; loop s
  in
  loop s;
  r
;;

let transfer_gen reader writer ~f =
  if !check_invariant then begin
    invariant reader;
    invariant writer;
  end;
  Deferred.create (fun result ->
    let rec loop () =
      read' reader
      >>> function
        | `Eof -> Ivar.fill result ()
        | `Ok inq -> f inq continue
    and continue outq =
      if is_closed writer
      then begin close_read reader; Ivar.fill result () end
      else write' writer outq >>> loop
    in
    loop ())
;;

let transfer' reader writer ~f = transfer_gen reader writer ~f:(fun q k -> f q >>> k)

let transfer reader writer ~f =
  transfer_gen reader writer ~f:(fun q k -> k (Q.map q ~f))
;;

let transfer_id reader writer = transfer_gen reader writer ~f:(fun q k -> k q)

let map' reader ~f =
  let (result, writer) = create () in
  upon (transfer' reader writer ~f) (fun () -> close writer);
  result
;;

let map reader ~f = map' reader ~f:(fun a -> return (Q.map a ~f))

let filter_map' reader ~f = map' reader ~f:(fun q -> Deferred.Queue.filter_map q ~f)

let filter_map  reader ~f = map' reader ~f:(fun q -> return (Q.filter_map q ~f))

let filter reader ~f = filter_map reader ~f:(fun x -> if f x then Some x else None)

let take_until input stop =
  let output, writer = create () in
  let rec loop () =
    choose [ choice stop (fun () -> `Stop);
             choice (values_available input) Fn.id;
           ]
    >>> function
      | `Stop | `Eof -> close writer
      | `Ok ->
        match read_now input with
        | `Eof -> close writer
        | `Nothing_available -> loop ()
        | `Ok q -> write' writer q >>> loop
  in
  loop ();
  output
;;

let read_all reader =
  let result = Q.create () in
  fold_gen reader ~init:() ~f:(fun () q loop -> Q.transfer ~src:q ~dst:result; loop ())
  >>| fun () ->
  result
;;

let to_list r = read_all r >>| Q.to_list

let interleave readers =
  let (reader, writer) = create () in
  let num_open = ref (List.length readers) in
  List.iter readers ~f:(fun reader ->
    if !check_invariant then invariant reader;
    upon (transfer_id reader writer) (fun () ->
      decr num_open;
      if !num_open = 0 then close writer));
  reader
;;

let of_list l =
  let reader, writer = create () in
  whenever (write' writer (Q.of_list l));
  close writer;
  reader
;;

let concat rs =
  let r, w = create () in
  upon (Deferred.List.iter rs ~f:(fun r -> transfer_id r w)) (fun () -> close w);
  r
;;
