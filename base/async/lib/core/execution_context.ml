open Core.Std
open Import

module Scheduler = Basic.Scheduler

include Basic.Execution_context

let create_like ?block_group ?monitor ?priority t =
  let get o z = match o with None -> z | Some x -> x in
  { block_group = get block_group t.block_group;
    monitor = get monitor t.monitor;
    priority = get priority t.priority;
  }
;;

