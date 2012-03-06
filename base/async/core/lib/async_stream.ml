open Core.Std
open Import
open Deferred_std

type ('a, 'execution_context) next_ = ('a, 'execution_context) Raw_stream.next =
  Nil | Cons of 'a * ('a, 'execution_context) Raw_stream.t

type 'a next = ('a, Execution_context.t) next_

type 'a t = ('a, Execution_context.t) Raw_stream.t with sexp_of

let next = Raw_stream.next

(* This line is needed in to avoid a dependency cycle when building with ocamlbuild, which
   considers both the '.ml' and the '.mli' as part of a single compilation unit. Without
   this line, async_stream.ml would depend on Monitor (while monitor.mli depends on
   Async_stream). *)
(* module Monitor = Raw_monitor *)

let first_exn t =
  next t
  >>| function
  | Nil -> failwith "Stream.first of empty stream"
  | Cons (x, _) -> x
;;

let fold' t ~init ~f =
  Deferred.create
    (fun result ->
      let rec loop t b =
        upon (next t) (function
        | Nil -> Ivar.fill result b
        | Cons (v, t) -> upon (f b v) (loop t))
      in
      loop t init)
;;

(* [fold] is implemented to avoid per-stream-element deferred overhead in the case when
   multiple stream elements are available simultaneously. *)
let fold t ~init ~f =
  Deferred.create
    (fun result ->
      let rec loop t b =
        match Deferred.peek t with
        | None -> upon t (fun next -> loop_next next b)
        | Some next -> loop_next next b
      and loop_next next b =
        match next with
        | Nil -> Ivar.fill result b
        | Cons (v, t) -> loop t (f b v)
      in
      loop t init)
;;

let length t = fold t ~init:0 ~f:(fun n _ -> n + 1)

let iter' t ~f = fold' t ~init:() ~f:(fun () v -> f v)

let closed t = iter' t ~f:(fun _ -> Deferred.unit)

let iter t ~f = whenever (iter' t ~f:(fun a -> f a; Deferred.unit))

let create f =
  let tail = Tail.create () in
  (* collect before calling [f], in case [f] immediately extends. *)
  let t = Tail.collect tail in
  f tail;
  t
;;

let unfold b ~f =
  create (fun tail ->
    let rec loop b =
      upon (f b) (function
        | None -> Tail.close_exn tail
        | Some (a, b) -> Tail.extend tail a; loop b)
    in
    loop b)
;;

let of_list l =
  create (fun tail ->
    List.iter l ~f:(fun x -> Tail.extend tail x);
    Tail.close_exn tail)
;;

let to_list s = fold' s ~init:[] ~f:(fun b a -> return (a :: b)) >>| List.rev

let copy_to_tail t tail = iter' t ~f:(fun a -> return (Tail.extend tail a))

let append t1 t2 =
  create (fun tail ->
    upon (copy_to_tail t1 tail) (fun () ->
      upon (copy_to_tail t2 tail) (fun () ->
        Tail.close_exn tail)))
;;

let concat t =
  create (fun tail ->
    upon (iter' t ~f:(fun t -> copy_to_tail t tail))
      (fun () -> Tail.close_exn tail))
;;

let filter' t ~f =
  create (fun tail ->
    upon (iter' t ~f:(fun v ->
      f v
      >>| (function false -> () | true -> Tail.extend tail v)))
      (fun () -> Tail.close_exn tail))
;;

let filter t ~f = filter' t ~f:(fun a -> return (f a))

let filter_map' t ~f =
  create (fun tail ->
    upon (iter' t ~f:(fun v ->
      f v >>| (function None -> () | Some v -> Tail.extend tail v)))
      (fun () -> Tail.close_exn tail))
;;

let filter_map t ~f = filter_map' t ~f:(fun a -> return (f a))

let map' t ~f =
  create (fun tail ->
    upon (iter' t ~f:(fun v -> f v >>| Tail.extend tail))
      (fun () -> Tail.close_exn tail))
;;

let map t ~f = map' t ~f:(fun a -> return (f a))

let first_n s n =
  create (fun tail ->
    let rec loop s n =
      if n = 0 then
        Tail.close_exn tail
      else
        upon (next s) (function
          | Nil -> Tail.close_exn tail
          | Cons (x, s) -> Tail.extend tail x; loop s (n - 1))
    in
    loop s n)
;;

let available_now t =
  let rec loop t ac =
    match Deferred.peek (next t) with
    | None | Some Nil -> (List.rev ac, t)
    | Some (Cons (x, t)) -> loop t (x :: ac)
  in
  loop t []
;;

let split ?(stop = Deferred.never ()) ?(f = (fun _ -> `Continue)) t =
  let reason_for_stopping = Ivar.create () in
  let prefix = Tail.create () in
  let finish v = Tail.close_exn prefix; Ivar.fill reason_for_stopping v in
  let rec loop t =
    choose [ choice stop (fun () -> `Stopped);
             choice (next t) (fun o -> `Next o);
           ]
    >>> function
      | `Stopped -> finish (`Stopped t)
      | `Next o ->
        match o with
        | Nil -> finish `End_of_stream
        | Cons (a, t) ->
          match f a with
          | `Continue -> Tail.extend prefix a; loop t
          | `Found b -> finish (`Found (b, t))
  in
  loop t;
  (Tail.collect prefix, Ivar.read reason_for_stopping)
;;

let find t ~f =
  let (_, found) = split t ~f:(fun a -> if f a then `Found a else `Continue) in
  found >>| (function
    | `Stopped _ -> assert false
    | `End_of_stream | `Found _ as x -> x)
;;

let ungroup t =
  create (fun tail ->
    upon (iter' t ~f:(fun l ->
      List.iter l ~f:(fun x -> Tail.extend tail x); Deferred.unit))
      (fun () -> Tail.close_exn tail))
;;

let interleave ts =
  create (fun tail ->
    (* The interleaved stream should be closed when the outer stream and all of
       the inner streams have been closed.  Keep a count of the number of open
       streams and close the interleaved stream when that count becomes
       zero. *)
    let num_open = ref 1 in (* 1 for the outer stream that is open *)
    let close () =
      num_open := !num_open - 1;
      if !num_open = 0 then Tail.close_exn tail;
    in
    let outer_closed =
      iter' ts ~f:(fun t ->
        num_open := !num_open + 1;
        upon (copy_to_tail t tail) close;
        Deferred.unit)
    in
    upon outer_closed close)
;;

let take_until t d =
  create (fun tail ->
    let rec loop t =
      upon (choose [choice d (fun () -> `Stop);
                    choice (next t) (fun z -> `Next z)])
        (function
          | `Stop | `Next Nil -> Tail.close_exn tail
          | `Next (Cons (x, t)) -> Tail.extend tail x; loop t)
    in
    loop t)
;;

let iter_durably' t ~f =
  Deferred.create (fun result ->
    let rec loop t =
      next t
      >>> function
        | Nil -> Ivar.fill result ()
        | Cons (x, t) ->
          Monitor.try_with_raise_rest (fun () -> f x)
          >>> fun z ->
          loop t;
          match z with
          | Ok () -> ()
          | Error e -> Monitor.send_exn (Monitor.current ()) e
    in
    loop t)
;;

let iter_durably_report_end t ~f =
  Deferred.create (fun result ->
    let rec loop t =
      next t
      >>> function
        | Nil -> Ivar.fill result ()
        | Cons (x, t) ->
          (* We immediately call [loop], thus making the iter durable.  Any exceptions
             raised by [f] will not prevent the loop from continuing, and will go to the
             monitor of whomever called [iter_durably_report_end]. *)
          loop t; f x
    in
    loop t)
;;

let iter_durably t ~f = Deferred.whenever (iter_durably_report_end t ~f)

let of_fun f = unfold () ~f:(fun () -> f () >>| fun a -> Some (a, ()))
