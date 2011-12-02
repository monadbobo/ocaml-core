open Core.Std
open Import

open Basic

include Basic.Tail

let sexp_of_t _ t = Sexp.Atom (if Ivar.is_empty t.next then "<open>" else "<closed>")

let collect t = Ivar.read t.next

let is_closed t = Ivar.is_full t.next

let fill_exn t v =
  if is_closed t then
    failwith "stream is closed"
  else
    Ivar.fill t.next v
;;

let close_exn t = fill_exn t Stream.Nil

let close_if_open t = if not (is_closed t) then Ivar.fill t.next Stream.Nil

let extend t v =
  let next = Ivar.create () in
  fill_exn t (Stream.Cons (v, Ivar.read next));
  t.next <- next;
;;
