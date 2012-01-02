open Core.Std
open Import

include Async_core.Block_group

module Scheduler = Raw_scheduler

let create ?min_reserved_threads ?max_reserved_threads () =
  Scheduler.create_block_group (Scheduler.the_one_and_only ())
    ?min_reserved_threads ?max_reserved_threads ()
