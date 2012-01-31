open OUnit;;

let () =
  Async_scheduler.Inline_tests.run ();
  Async_extra.Inline_tests.run ()
