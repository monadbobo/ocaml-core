include Bin_prot.Binable
open Sexplib.Std
open Bin_prot.Std

module Of_stringable (M : Stringable.S) =
  Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = string with bin_io
    end
    type t = M.t
    let to_binable = M.to_string

    (* Wrap exception for improved diagnostics. *)
    exception Of_binable of string * exn with sexp
    let of_binable s =
      try
        M.of_string s
      with x ->
        raise (Of_binable (s, x))
  end)

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t



let of_bigstring (type a) m bigstring =
  let module M = (val m : S with type t = a) in
  let pos_ref = ref 0 in
  let t = M.bin_read_t bigstring ~pos_ref in
  assert (!pos_ref = Array1.dim bigstring);
  t
;;

let to_bigstring (type a) m t =
  let module M = (val m : S with type t = a) in
  let size = M.bin_size_t t in
  let bigstring = Array1.create Bigarray.char Bigarray.c_layout size in
  let pos = M.bin_write_t bigstring ~pos:0 t in
  assert (pos = size);
  bigstring
;;
