(* Standard Library *)
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val ifprintf : 'a -> ('b, 'a, unit) format -> 'b
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kfprintf : (out_channel -> 'a) -> out_channel ->
              ('b, out_channel, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kbprintf : (Buffer.t -> 'a) -> Buffer.t ->
              ('b, Buffer.t, unit, 'a) format4 -> 'b

(**
  {6 Formatting error and exit functions}
*)
(* raises Failure *)
val failwithf    : ('a, unit, string, unit -> _) format4 -> 'a
(* raises Invalid_arg *)
val invalid_argf : ('a, unit, string, unit -> _) format4 -> 'a
(* prints to standard error, then exits with code 1 *)
val exitf        : ('a, unit, string, unit -> _) format4 -> 'a
