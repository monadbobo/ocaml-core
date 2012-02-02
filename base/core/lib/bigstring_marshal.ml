INCLUDE "config.mlh"
IFDEF LINUX_EXT THEN

open Sexplib.Conv
open Result.Export
open Bigstring

(* Marshalling to/from bigstrings *)

external unsafe_marshal_blit :
  'a -> pos : int -> len : int -> t -> Marshal.extern_flags list -> int
  = "bigstring_marshal_blit_stub"

let marshal_blit ?(flags = []) v ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"marshal" bstr ~pos ~len;
  unsafe_marshal_blit v ~pos ~len bstr flags
;;

external marshal : 'a -> Marshal.extern_flags list -> t
  = "bigstring_marshal_stub"

let marshal ?(flags = []) x = marshal x flags

external unsafe_marshal_data_size :
  pos : int -> t -> int = "bigstring_marshal_data_size_stub"

let marshal_data_size ?(pos = 0) bstr =
  if pos < 0 || pos > length bstr - Marshal.header_size
  then invalid_arg "Bigstring.marshal_data_size"
  else unsafe_marshal_data_size ~pos bstr

external unsafe_unmarshal :
  pos : int -> len : int -> t -> 'a = "bigstring_unmarshal_stub"

let unmarshal_next ?pos bstr =
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
        if pos < 0 then invalid_arg "Bigstring.unmarshal: pos < 0"
        else pos
  in
  let len = length bstr in
  let len_header = len - Marshal.header_size in
  if pos > len_header
  then invalid_arg "Bigstring.unmarshal: pos > len - header"
  else
    let data_len = unsafe_marshal_data_size ~pos bstr in
    let block_len = Marshal.header_size + data_len in
    let next_pos = pos + block_len in
    if next_pos > len
    then invalid_arg "Bigstring.unmarshal: pos + block_len > len"
    else
      let v = unsafe_unmarshal ~pos ~len:block_len bstr in
      v, next_pos
;;

let unmarshal ?pos bstr = fst (unmarshal_next ?pos bstr)

let skip ?pos bstr =
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
        if pos < 0 then invalid_arg "Bigstring.skip: pos < 0"
        else pos
  in
  let len = length bstr in
  let len_header = len - Marshal.header_size in
  if pos > len_header
  then invalid_arg "Bigstring.skip: pos > len - header"
  else
    let data_len = unsafe_marshal_data_size ~pos bstr in
    let block_len = Marshal.header_size + data_len in
    let next_pos = pos + block_len in
    if next_pos > len
    then invalid_arg "Bigstring.skip: pos + block_len > len"
    else next_pos
;;

let marshal_to_sock ?buf ?flags sock v =
  let buf, len =
    match buf with
    | None ->
        let buf = marshal ?flags v in
        buf, length buf
    | Some buf -> buf, marshal_blit ?flags v buf
  in
  let f = Or_error.ok_exn really_send_no_sigpipe in
  f sock buf ~len
;;

let unmarshal_from_sock ?buf sock =
  match buf with
  | None ->
      let buf_len = 4096 in
      let buf = create buf_len in
      really_recv sock ~len:Marshal.header_size buf;
      let data_len = marshal_data_size buf in
      let all_len = Marshal.header_size + data_len in
      let buf =
        if all_len <= buf_len then buf
        else create all_len
      in
      really_recv sock ~pos:Marshal.header_size ~len:data_len buf;
      unsafe_unmarshal ~pos:0 ~len:all_len buf
  | Some buf ->
      let buf_len = length buf in
      if buf_len < Marshal.header_size then
        failwith "Bigstring.unmarshal_from_sock: buffer cannot hold header";
      really_recv sock ~len:Marshal.header_size buf;
      let data_len = marshal_data_size buf in
      let all_len = Marshal.header_size + data_len in
      if all_len > buf_len then
        failwith
          "Bigstring.unmarshal_from_sock: buffer cannot hold header + data";
      really_recv sock ~pos:Marshal.header_size ~len:data_len buf;
      unsafe_unmarshal ~pos:0 ~len:all_len buf
;;

let marshal             = Ok marshal
let marshal_blit        = Ok marshal_blit
let marshal_data_size   = Ok marshal_data_size
let marshal_to_sock     = Ok marshal_to_sock
let skip                = Ok skip
let unmarshal           = Ok unmarshal
let unmarshal_from_sock = Ok unmarshal_from_sock
let unmarshal_next      = Ok unmarshal_next

ELSE

open Std_internal

let marshal             = unimplemented "Bigstring_marshal.marshal"
let marshal_blit        = unimplemented "Bigstring_marshal.marshal_blit"
let marshal_data_size   = unimplemented "Bigstring_marshal.marshal_data_size"
let marshal_to_sock     = unimplemented "Bigstring_marshal.marshal_to_sock"
let skip                = unimplemented "Bigstring_marshal.skip"
let unmarshal           = unimplemented "Bigstring_marshal.unmarshal"
let unmarshal_from_sock = unimplemented "Bigstring_marshal.unmarshal_from_sock"
let unmarshal_next      = unimplemented "Bigstring_marshal.unmarshal_next"

ENDIF
