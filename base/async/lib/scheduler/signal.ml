open Core.Std
open Import

include Core.Std.Signal

module Scheduler = Raw_scheduler

let the_one_and_only = Scheduler.the_one_and_only

let handle ?(stop = never ()) ts ~f =
  let scheduler = the_one_and_only () in
  List.iter ts ~f:(fun t -> Scheduler.handle_signal scheduler t);
  let module S = Stream in
  let rec loop delivered =
    choose [ choice stop (fun () -> `Stop);
             choice (S.next delivered) (fun n -> `Next n);
           ]
    >>> function
      | `Stop -> ()
      | `Next next ->
        if not (Deferred.is_determined stop) then begin
          match next with
          | S.Nil ->
            (* The stream of signals never ends. *)
            assert false
          | S.Cons (t, delivered) ->
            if List.mem ts t then f t;
            loop delivered;
        end
  in
  loop (Scheduler.signals scheduler)
;;

let standard =
  (* Can't do [kill, stop] because it's not allowed to handle them.
     Don't do [segv, vtalrm] because they already have a handler.
     Don't do [fpe] because we want to hear about it.
     Don't do [prof] so that we can profile things with -p.
  *)
  [
    abrt; alrm; chld; cont; hup; int; quit; term; tstp; ttin; ttou;
    usr1; usr2;
  ]
;;
