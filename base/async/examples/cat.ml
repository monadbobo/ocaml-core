open Jane.Std
open Async.Std

module Fd = Unix.Fd

let cat ~input ~output =
  let reader = Reader.create input in
  let writer = Writer.create ~raise_epipe:false output in
  let buf = String.create 4096 in
  let rec loop () =
    upon
      (choose
        [
          choice (Reader.read reader buf) (fun r -> `Reader r);
          choice (Writer.got_epipe writer) (fun () -> `Epipe);
        ])
    (function
      | `Reader r ->
        begin
          match r with
          | `Eof -> upon (Writer.flushed writer) (fun _ ->
            never_returns (shutdown_dont_return 0))
          | `Ok len ->
              Writer.write_substring writer (Substring.create buf ~pos:0 ~len);
              loop ()
        end
      | `Epipe -> never_returns (shutdown_dont_return 0))
  in
  loop ()
;;

let () = cat ~input:(Fd.stdin ()) ~output:(Fd.stdout ())

let () = never_returns (Scheduler.go ())
