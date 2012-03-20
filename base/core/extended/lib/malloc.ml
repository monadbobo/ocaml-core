(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type_conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Sexplib.Std
open Bin_prot.Std

type mallinfo = {
  arena : int;
  ordblks : int;
  smblks : int;
  hblks : int;
  hblkhd : int;
  usmblks : int;
  fsmblks : int;
  uordblks : int;
  fordblks : int;
  keepcost : int;
} with sexp, bin_io

external mallinfo : unit -> mallinfo = "malloc_mallinfo_stub"

type opt =
  | TRIM_THRESHOLD
  | TOP_PAD
  | MMAP_THRESHOLD
  | MMAP_MAX
  | CHECK_ACTION
(*   | PERTURB *)
with sexp, bin_io

external mallopt : opt -> int -> unit = "malloc_mallopt_stub"

external malloc_trim : int -> unit = "malloc_trim_stub"

external malloc_stats : unit -> unit = "malloc_stats_stub"
