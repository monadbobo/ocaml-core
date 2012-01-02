open Core.Std
open Import
open Deferred_std

module Scheduler = Basic.Scheduler
module Stream = Basic.Stream

include Basic.Monitor

let debug = Debug.debug

let name t =
  match t.name_opt with
  | Some s -> s
  | None -> Int.to_string (id t)
;;

let next_id =
  let r = ref 0 in
  fun () -> r := !r + 1; !r
;;

let create_with_parent ?name:name_opt parent =
  let t =
    { name_opt;
      id = next_id ();
      parent;
      errors = Tail.create ();
      has_seen_error = false;
      someone_is_listening = false;
    }
  in
  if debug then
    Debug.print "creating monitor %s with parent %s"
      (name t) (match parent with None -> "<none>" | Some p -> name p);
  t
;;

let main = create_with_parent ~name:"main" None

type monitor = t with sexp_of

module Error = struct
  type t =
    { exn : exn;
      backtrace : string sexp_list;
      monitor : monitor;
    }
  with fields, sexp_of
end

exception Error_ of Error.t with sexp

exception Unhandled_exception of [ `Pid of Pid.t ] * exn with sexp

exception Shutdown

let extract_exn exn =
  match exn with
  | Error_ error -> error.Error.exn
  | exn -> exn
;;

let send_exn t exn ~backtrace =
  let backtrace =
    let split backtrace = String.split backtrace ~on:'\n' in
    match backtrace with
    | `None -> []
    | `Get -> split (Exn.backtrace ())
    | `This b -> split b
  in
  let is_shutdown = exn = Shutdown in
  let exn =
    match exn with
    | Error_ _ -> exn
    | _ -> Error_ { Error. exn; backtrace; monitor = t }
  in
  t.has_seen_error <- true;
  let rec loop t =
    if t.someone_is_listening then
      Tail.extend t.errors exn
    else
      match t.parent with
      | Some monitor -> loop monitor
      (* Swallow shutdown errors that reach the top. *)
      | None when is_shutdown -> ()
      | None ->
        (* Do not change this branch to print the exception or exit.  Having the
           scheduler raise an uncaught exception is the necessary behavior for programs
           that call [Scheduler.go] and want to handle it. *)
        Scheduler.t.Scheduler.uncaught_exception <-
          Some (Unhandled_exception (`Pid (Unix.getpid ()), exn));
  in
  loop t
;;

let with_execution_context tmp_context ~f =
  let old_context = Scheduler.current_execution_context () in
  Scheduler.set_execution_context tmp_context;
  protect ~f ~finally:(fun () -> Scheduler.set_execution_context old_context);
;;

let within_context context f =
  with_execution_context context
    ~f:(fun () ->
      match Result.try_with f with
      | Ok x -> Ok x
      | Error exn ->
        send_exn context.Execution_context.monitor exn ~backtrace:`Get;
        Error ())
;;

let within_gen ?block_group ?monitor ?priority f =
  let tmp_context =
    Execution_context.create_like (Scheduler.current_execution_context ())
      ?block_group ?monitor ?priority
  in
  within_context tmp_context f
;;

type 'a with_options =
     ?block_group:Block_group.t
  -> ?monitor:t
  -> ?priority:Priority.t
  -> 'a

let within' ?block_group ?monitor ?priority f =
  match within_gen  ?block_group ?monitor ?priority f with
  | Error () -> Deferred.never ()
  | Ok d -> d
;;

let within_v ?block_group ?monitor ?priority f =
  match within_gen  ?block_group ?monitor ?priority f with
  | Error () -> None
  | Ok x -> Some x
;;

let within ?block_group ?monitor ?priority f =
  match within_gen  ?block_group ?monitor ?priority f with
  | Error () -> ()
  | Ok () -> ()
;;

let schedule ?block_group ?monitor ?priority work =
  Scheduler.add_job
    (Execution_context.create_like (Scheduler.current_execution_context ())
       ?block_group ?monitor ?priority)
    work
;;

let schedule' ?block_group ?monitor ?priority work =
  Deferred.create (fun i ->
    Scheduler.add_job
      (Execution_context.create_like (Scheduler.current_execution_context ())
         ?block_group ?monitor ?priority)
      (fun () -> upon (work ()) (fun a -> Ivar.fill i a)))

;;

let current () = (Scheduler.current_execution_context ()).Execution_context.monitor

let errors t =
  t.someone_is_listening <- true;
  Tail.collect t.errors;
;;

let error t =
  let module S = Stream in
  S.next (Tail.collect t.errors) >>| function
    | S.Nil -> assert false
    | S.Cons (error, _) -> error
;;

let create ?name () =
  let parent = current () in
  create_with_parent ?name (Some parent);
;;

let try_with ?(name="try_with") ~reraise f =
  let module S = Stream in
  let parent = current () in
  let monitor = create_with_parent ~name (Some parent) in
  let errors = errors monitor in
  choose [ choice (schedule' ~monitor f)
             (fun x -> (Ok x, errors));
           choice (S.next errors)
             (function
               | S.Nil -> assert false
               | S.Cons (err, errors) -> (Error err, errors));
         ]
  >>| fun (res, errors) ->
  if reraise then S.iter errors ~f:(fun e -> send_exn parent e ~backtrace:`None);
  res
;;

let send_exn t ?backtrace exn =
  send_exn t exn
    ~backtrace:(match backtrace with | None -> `None | Some (`Get | `This _ as x) -> x)
;;

let try_with_raise_rest ?name f = try_with ?name ~reraise:true f

let try_with ?name f = try_with ?name ~reraise:false f

let protect ?name f ~finally =
  try_with ?name f >>= fun r ->
    let name = Option.map name ~f:(sprintf "%s::finally") in
    try_with ?name finally >>| fun fr ->
      match fr, r with
      | Error finally_e, Error e ->
        fail "Async finally" (e, finally_e) <:sexp_of< exn * exn >>
      | Error e        , Ok _
      | Ok ()          , Error e -> raise e
      | Ok ()          , Ok r    -> r
;;

let handle_errors ?name f handler =
  let monitor = create ?name () in
  Stream.iter (errors monitor) ~f:handler;
  within' ~monitor f;
;;

let catch_stream ?name f =
  let monitor = create ?name () in
  within ~monitor f;
  errors monitor
;;

let catch ?name f =
  let module S = Stream in
  S.next (catch_stream ?name f)
  >>| function
    | S.Cons (x, _) -> x
    | S.Nil -> failwith "Monitor.catch got unexpected empty stream"
;;
