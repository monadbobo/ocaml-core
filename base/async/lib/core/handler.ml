open Core.Std
open Import

include Basic.Handler

let install t d =
  let u = Basic.Deferred.install_removable_handler d t in
  fun () -> Basic.Unregister.unregister u;
;;
