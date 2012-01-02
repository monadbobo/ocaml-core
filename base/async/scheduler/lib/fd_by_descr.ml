open Core.Std
open Import

module Fd = Raw_fd

module File_descr = Unix.File_descr

type t = (File_descr.t, Fd.t) Bounded_int_table.t with sexp_of

let create_fd t kind file_descr ~name =
  match Bounded_int_table.find t file_descr with
  | Some fd -> fail "attempt to overwrite existing fd" (fd, t) <:sexp_of< Fd.t * t >>
  | None ->
    let supports_nonblock =
        (* Do not change blocking status of TTYs!  This would affect all processes
           currently attached to that TTY and even persist after this process
           terminates. *)
      if Core.Std.Unix.isatty file_descr then
        false
      else begin
        let module K = Fd.Kind in
        begin match kind with
        | K.File
            (* No point in setting nonblocking for files.  Unix doesn't care. *)
          -> false
        | K.Char
        | K.Fifo
          (* `Unconnected sockets support nonblocking so we can connect() them.
             `Passive     sockets support nonblocking so we can accept() them.
             `Active      sockets support nonblocking so we can read() and write() them. *)
        | K.Socket (`Unconnected | `Bound | `Passive | `Active)
          -> true
        end
      end
    in
    let fd =
      { Fd.
        name;
        file_descr;
        kind;
        supports_nonblock;
        have_set_nonblock = false;
        state = Fd.State.Open;
        num_active_syscalls = 0;
        close_finished = Ivar.create ();
      }
    in
    Bounded_int_table.add_exn t ~key:file_descr ~data:fd;
    fd;
;;

