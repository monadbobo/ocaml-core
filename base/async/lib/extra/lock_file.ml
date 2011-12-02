open Core.Std
open Import

let (>>|) = Deferred.(>>|)

let create ?message ?close_on_exec path =
  In_thread.run (fun () ->
    Core.Std.Lock_file.create ?message ?close_on_exec path)
;;

let create_exn ?message ?close_on_exec path =
  create ?message ?close_on_exec path
  >>| fun b ->
  if not b then fail "Lock_file.create" path <:sexp_of< string >>
;;

let waiting_create ?message path =
  In_thread.run (fun () -> Core.Std.Lock_file.blocking_create ?message path)
;;

let is_locked path =
  In_thread.run (fun () -> Core.Std.Lock_file.is_locked path)
;;
