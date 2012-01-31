open Core.Std
open Import

module Signal = Core.Std.Signal

module Handler = struct
  type t = Signal.t -> unit
end

module Handlers = struct
  type t = { bag : Handler.t sexp_opaque Bag.t } with sexp_of

  let create () = { bag = Bag.create () }

  let deliver t signal = Bag.iter t.bag ~f:(fun f -> f signal)

  let add t handler = Bag.add t.bag handler

  let remove t handler_elt = Bag.remove t.bag handler_elt

  let deliver t signal = Bag.iter t.bag ~f:(fun handler -> handler signal)
end

type t =
  { handlers_by_signal : Handlers.t Signal.Table.t;
    delivered : (Signal.t * Handlers.t) Thread_safe_queue.t sexp_opaque;
    thread_safe_notify_signal_delivered : unit -> unit;
  }
with sexp_of

let create ~thread_safe_notify_signal_delivered =
  { handlers_by_signal = Signal.Table.create ();
    delivered = Thread_safe_queue.create ();
    thread_safe_notify_signal_delivered;
  }
;;

type z = (Handlers.t * Handler.t Bag.Elt.t) list

let get_handlers t signal =
  Hashtbl.find_or_add t.handlers_by_signal signal ~default:(fun () ->
    let handlers = Handlers.create () in
    Signal.handle signal (fun _ ->
      (* Must be thread safe. *)
      Thread_safe_queue.enqueue t.delivered (signal, handlers);
      t.thread_safe_notify_signal_delivered ());
    handlers)
;;

let handle_signal t signal = ignore (get_handlers t signal : Handlers.t)

let install_handler t signals handler =
  List.map signals ~f:(fun signal ->
    let handlers = get_handlers t signal in
    (handlers, Handlers.add handlers handler))
;;

let remove_handler _t z = List.iter z ~f:(fun (handlers, handler_elt) ->
  Handlers.remove handlers handler_elt)
;;

let handle_delivered t =
  Thread_safe_queue.dequeue_until_empty t.delivered (fun (signal, handlers) ->
    Handlers.deliver handlers signal)
;;
