(** Regression test runner. *)

open Core.Std;;
open Qtest_lib.Std;;

let tests =
  Test_handler.tests
  @ Writer_test.tests
  @ Pipe_test.tests
  @ Tcp_serve.tests
  @ Rpc_test.tests
  @ Bind_test.tests
  @ Unpack_sequence_test.tests
  @ Reader_test.tests
  @ Rpc_canary_test.tests

let () =
  let module Runner = Qtest_lib.Std.Runner.Make(Version_util) in
  Runner.main tests
