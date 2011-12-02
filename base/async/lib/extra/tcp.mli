open Core.Std
open Import

(** [connect_sock ~host ~port] opens a TCP connection to the specified hostname
    and port, returning the socket.

    Any errors in the connection will be reported to the monitor that was current
    when connect was called. *)
val connect_sock :
  host:string -> port:int -> ([ `Active ], Socket.inet) Socket.t Deferred.t

(** [connect ~host ~port] is a convenience wrapper around [connect_sock] that
    returns a reader and writer on the socket. *)
val connect :
     ?max_buffer_age:Time.Span.t
  -> ?timeout:Time.Span.t
  -> host:string
  -> port:int
  -> unit
  -> (Reader.t * Writer.t) Deferred.t

(** [with_connection ~host ~port f] looks up host from a string (using DNS as
    needed), connects, then calls [f] passing in a reader and a writer for the
    connected socket.  When the deferred returned by [f] is determined, or any
    exception is thrown, the socket (and reader and writer) are closed.  The
    return deferred is fulfilled after f has finished processing and the file
    descriptor for the socket is closed. *)
val with_connection :
  ?timeout: Time.Span.t
  -> ?max_buffer_age:Time.Span.t
  -> host:string
  -> port:int
  -> (Reader.t -> Writer.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [serve ~port handler] starts a server on the specified port.  The return
    value becomes determined once the socket is ready to accept connections.
    [serve] calls [handler (address, reader, writer)] for each client that
    connects.  If the deferred returned by [handler] is ever determined, or
    [handler] raises an exception, then [reader] and [writer] are closed.

    [max_pending_connections] is the maximum number of clients that can have a
    connection pending, as with [Async.Std.Unix.Socket.listen].  Additional
    connections will be rejected.

    [max_buffer_age] passes on to the underlying writer option of the same name.

    [on_handler_error] determines what happens if the handler throws an
    exception. *)
val serve :
  ?max_connections:int
  -> ?max_pending_connections:int
  -> ?max_buffer_age:Time.Span.t
  -> port:int
  -> on_handler_error:[ `Raise
                      | `Ignore
                      | `Call of (Socket.inet -> exn -> unit)
                      ]
  -> (Socket.inet -> Reader.t -> Writer.t -> unit Deferred.t)
  -> unit Deferred.t
