open Core.Std
open Async.Std
open Async_extended.Std
open Qtest_lib.Std
open Print

module Q = Queue

let tests = ref []

let test f = tests := f :: !tests

let show_reader reader =
  eprintf "%s\n"
    (Sexp.to_string_hum (Pipe.Reader.sexp_of_t Int.sexp_of_t reader));
;;

let () =
  test (fun () ->
    let (_reader, writer) = Pipe.create () in
    assert (not (Pipe.is_closed writer));
    Pipe.close writer;
    assert (Pipe.is_closed writer);
    Deferred.unit);

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    Pipe.close writer;
    Pipe.read_all reader
    >>| fun q ->
    assert (Q.to_list q = []));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    whenever (Pipe.write writer 13);
    Pipe.close writer;
    Pipe.read_all reader
    >>| fun q ->
    assert (Q.to_list q = [13]));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    let w x = whenever (Pipe.write writer x) in
    w 13;
    upon (after (sec 0.1)) (fun () -> w 14; Pipe.close writer);
    Pipe.read' reader
    >>= function
      | `Eof -> assert false
      | `Ok q ->
        assert (Q.to_list q = [13]);
        Pipe.read_all reader
        >>| fun q ->
        assert (Q.to_list q = [14]));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    whenever (Pipe.write writer 13);
    Pipe.close writer;
    let read_happened = ref false in
    Pipe.close writer;
    upon (Pipe.flushed writer) (function
      | `Reader_closed -> assert false
      | `Ok -> upon (after (sec 0.1)) (fun () -> assert !read_happened));
    after (sec 0.1)
    >>| fun () ->
    upon (Pipe.read_all reader) (fun q ->
      assert (Q.length q = 1);
      read_happened := true));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    whenever (Pipe.write' writer (Q.of_list [12; 13]));
    Pipe.close writer;
    Pipe.read_all (Pipe.map reader ~f:(fun x -> x + 1))
    >>| fun q ->
    assert (Q.to_list q = [13; 14]));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    whenever (Pipe.write' writer (Q.of_list [12; 13; 14]));
    Pipe.close writer;
    Pipe.read_at_most reader ~num_values:2
    >>| function
      | `Eof -> assert false
      | `Ok q ->
        assert (Q.to_list q = [12; 13]));

  test (fun () ->
    let (reader, writer) = Pipe.create () in
    whenever (Pipe.write' writer (Q.of_list [12; 13; 14]));
    Pipe.close writer;
    Pipe.read_at_most reader ~num_values:4
    >>| function
      | `Eof -> assert false
      | `Ok q ->
        assert (Q.to_list q = [12; 13; 14]));

  test (fun () ->
    let s = Stream.of_list [12; 13; 14] in
    let w = Pipe.of_stream s in
    Pipe.read_all w
    >>| fun q ->
    assert (Q.to_list q = [12; 13; 14])
    );
;;

let test_pipe () =
  Pipe.check_invariant := true;
  Pipe.show_debug_messages := false;
  Deferred.all_unit (List.map !tests ~f:(fun f -> f ()))
;;

let tests =
  ["Pipe_test", test_pipe; ]
;;
