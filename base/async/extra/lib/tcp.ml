open Core.Std
open Import

module Host = Unix.Host
module Socket = Unix.Socket

let create_socket () =
  let s = Socket.create Socket.Type.tcp in
  Unix.set_close_on_exec (Unix.Socket.fd s);
  s
;;

let close_sock_on_error s f =
  try_with ~name:"Tcp.close_sock_on_error" f
  >>| function
    | Ok v -> v
    | Error e ->
      (* [close] may fail, but we don't really care, since it will fail
         asynchronously.  The error we really care about is [e], and the
         [raise_error] will cause the current monitor to see that. *)
      whenever (Unix.close (Socket.fd s));
      raise e;
;;

let reader_writer_of_sock ?max_buffer_age ?reader_buffer_size s =
  let fd = Socket.fd s in
  (* If max_buffer_age is not specified, use the default in Writer. *)
  let buffer_age_limit =
    match max_buffer_age with
    | None     -> None
    | Some age -> Some (`At_most age)
  in
  (Reader.create ?buf_len:reader_buffer_size fd,
   Writer.create ?buffer_age_limit fd)
;;

exception Connection_attempt_aborted of string * int with sexp

let connect_sock ?(interrupt=Clock.after (sec 10.)) ~host ~port () =
  Deferred.create (fun result ->
    Unix.Inet_addr.of_string_or_getbyname host
    >>> fun inet_addr ->
    let addr = Socket.Address.inet inet_addr ~port in
    let s = create_socket () in
    close_sock_on_error s (fun () ->
      Socket.connect_interruptible s addr ~interrupt)
    >>> function
      | `Ok s -> Ivar.fill result s
      | `Interrupted ->
        whenever (Unix.close (Socket.fd s));
        raise (Connection_attempt_aborted (host, port)))
;;

let close_connection r w =
  Writer.close w ~force_close:(Clock.after (sec 30.))
  >>= fun () ->
  Reader.close r
;;

let connect ?max_buffer_age ?interrupt ?reader_buffer_size ~host ~port () =
  connect_sock ?interrupt ~host ~port ()
  >>| fun s ->
  reader_writer_of_sock ?max_buffer_age ?reader_buffer_size s
;;

let collect_errors writer f =
  let monitor = Writer.monitor writer in
  ignore (Monitor.errors monitor); (* don't propagate errors up, we handle them here *)
  choose [
    choice (Monitor.error monitor) (fun e -> Error e);
    choice (try_with ~name:"Tcp.collect_errors" f) Fn.id;
  ]
;;

let with_connection ?interrupt ?max_buffer_age ~host ~port f =
  connect_sock ?interrupt ~host ~port ()
  >>= fun s ->
  let r,w    = reader_writer_of_sock ?max_buffer_age s in
  let res    = collect_errors w (fun () -> f r w) in
  Deferred.choose_ident [
    res >>| (fun (_ : ('a, exn) Result.t) -> ());
    Reader.closed r;
    Writer.close_finished w;
  ]
  >>= fun () ->
  close_connection r w
  >>= fun () ->
  res >>| function
  | Ok v -> v
  | Error e -> raise (Monitor.extract_exn e)
;;

let handle_client ?max_buffer_age s addr f =
  let r, w = reader_writer_of_sock ?max_buffer_age s in
  collect_errors w (fun () -> f addr r w)
  >>= fun res ->
  close_connection r w
  >>| fun () ->
  res
;;

exception Tcp_server_negative_max_connections of int with sexp

let serve ?(max_connections=10_000) ?max_pending_connections ?max_buffer_age ~port
    ~on_handler_error handler =
  Deferred.create (fun ready ->
    if max_connections <= 0 then
      raise (Tcp_server_negative_max_connections max_connections);
    let s = create_socket () in
    close_sock_on_error s (fun () ->
      Socket.setopt s Socket.Opt.reuseaddr true;
      Socket.bind s (Socket.Address.inet_addr_any ~port)
      >>| Socket.listen ?max_pending_connections)
    >>> fun s ->
    Ivar.fill ready ();
    let num_connections   = ref 0 in
    let accept_is_pending = ref false in
    let rec accept_loop () =
      if !num_connections < max_connections && not !accept_is_pending then begin
        accept_is_pending := true;
        Socket.accept s
        >>> fun (client_s, addr) ->
        accept_is_pending := false;
        incr num_connections;
        accept_loop ();
        handle_client ?max_buffer_age client_s addr handler
        >>> fun res ->
        begin match res with
        | Ok () -> ()
        | Error e ->
          match on_handler_error with
          | `Ignore -> ()
          | `Raise  -> raise e
          | `Call f -> f addr e
        end;
        decr num_connections;
        accept_loop ()
      end
    in
    accept_loop ())
;;

let connect_sock ~host ~port = connect_sock ~host ~port ()
