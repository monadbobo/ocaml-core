open Core.Std

type t =
  { id : int;
    work : unit -> unit;
  }
with sexp_of

let id t = t.id

let run t = t.work ()

let next_id =
  let r = ref 0 in
  (fun () -> incr r; !r);
;;

let create work =
  { id = next_id ();
    work;
  }
;;
