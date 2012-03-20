(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type_conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

open Core.Std
open Unix

external setresuid : ruid:int -> euid:int -> suid:int -> unit = "linux_setresuid_stub"

let setresuid ?(ruid= -1) ?(euid= -1) ?(suid= -1) () =
  setresuid ~ruid ~euid ~suid

type uids = {
  ruid:int;
  euid:int;
  suid:int
} with sexp,bin_io

external getresuid : unit -> uids = "linux_getresuid_stub"


(* Splicing - zero-copies between kernel buffers *)


module Splice = struct
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io
  type flags

  external make_flags : flag array -> flags = "linux_splice_make_flags_stub"

  external unsafe_splice :
    bool ->
    fd_in : File_descr.t -> off_in : int ->
    fd_out : File_descr.t -> off_out : int ->
    len : int ->
    flags
    -> int * int * int = "linux_splice_stub_bc" "linux_splice_stub"

  let splice
        ?(assume_fd_is_nonblocking = false)
        ~fd_in ?off_in
        ~fd_out ?off_out ~len flags =
    let off_in =
      match off_in with
      | None -> -1
      | Some off_in when off_in < 0 -> invalid_arg "Splice.splice: off_in < 0"
      | Some off_in -> off_in
    in
    let off_out =
      match off_out with
      | None -> -1
      | Some off_out when off_out < 0 ->
          invalid_arg "Splice.splice: off_out < 0"
      | Some off_out -> off_out
    in
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_splice assume_fd_is_nonblocking ~fd_in ~off_in ~fd_out ~off_out ~len flags

  external unsafe_tee :
    bool -> fd_in : File_descr.t -> fd_out : File_descr.t -> int -> flags -> int
    = "linux_tee_stub"

  let tee ?(assume_fd_is_nonblocking = false) ~fd_in ~fd_out len flags =
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_tee assume_fd_is_nonblocking ~fd_in ~fd_out len flags

  external unsafe_vmsplice :
    bool -> File_descr.t -> int -> flags -> int = "linux_vmsplice_stub"

  let vmsplice ?(assume_fd_is_nonblocking = false) fd iovecs ?count flags =
    let count =
      match count with
      | None -> Array.length iovecs
      | Some count ->
          if count < 0 then invalid_arg "Splice.vmsplice: count < 0";
          let n_iovecs = Array.length iovecs in
          if count > n_iovecs then
            invalid_arg "Splice.vmsplice: count > n_iovecs";
          count
    in
    unsafe_vmsplice assume_fd_is_nonblocking fd count flags
end

module Statfs = struct
  module Raw = struct
    type t =
      {
        f_type    : int;
        f_bsize   : int;
        f_blocks  : int;
        f_bfree   : int;
        f_bavail  : int;
        f_files   : int;
        f_ffree   : int;
        f_namelen : int;
      }
    ;;
  end
  type f_type =
        ADFS_SUPER_MAGIC | AFFS_SUPER_MAGIC | BEFS_SUPER_MAGIC | BFS_MAGIC
      | CIFS_MAGIC_NUMBER | CODA_SUPER_MAGIC | COH_SUPER_MAGIC | CRAMFS_MAGIC
      | DEVFS_SUPER_MAGIC | EFS_SUPER_MAGIC | EXT_SUPER_MAGIC | EXT2_OLD_SUPER_MAGIC
      | EXT2_SUPER_MAGIC | EXT3_SUPER_MAGIC | HFS_SUPER_MAGIC | HPFS_SUPER_MAGIC
      | HUGETLBFS_MAGIC | ISOFS_SUPER_MAGIC | JFFS2_SUPER_MAGIC | JFS_SUPER_MAGIC
      | MINIX_SUPER_MAGIC | MINIX_SUPER_MAGIC2 | MINIX2_SUPER_MAGIC | MINIX2_SUPER_MAGIC2
      | MSDOS_SUPER_MAGIC | NCP_SUPER_MAGIC | NFS_SUPER_MAGIC | NTFS_SB_MAGIC
      | UNKNOWN_SUPER_MAGIC of int
  ;;
  type t =
    {
      f_type    : f_type;
      f_bsize   : int;
      f_blocks  : int;
      f_bfree   : int;
      f_bavail  : int;
      f_files   : int;
      f_ffree   : int;
      f_namelen : int;
    }
  ;;
  let of_rawstatfs raw =
    {
      f_type =
         begin match raw.Raw.f_type with
         | 0xadf5     -> ADFS_SUPER_MAGIC
         | 0xADFF     -> AFFS_SUPER_MAGIC
         | 0x42465331 -> BEFS_SUPER_MAGIC
         | 0x1BADFACE -> BFS_MAGIC
         | 0xFF534D42 -> CIFS_MAGIC_NUMBER
         | 0x73757245 -> CODA_SUPER_MAGIC
         | 0x012FF7B7 -> COH_SUPER_MAGIC
         | 0x28cd3d45 -> CRAMFS_MAGIC
         | 0x1373     -> DEVFS_SUPER_MAGIC
         | 0x00414A53 -> EFS_SUPER_MAGIC
         | 0x137D     -> EXT_SUPER_MAGIC
         | 0xEF51     -> EXT2_OLD_SUPER_MAGIC
         | 0xEF53     -> EXT2_SUPER_MAGIC
(*       | 0xEF53     -> EXT3_SUPER_MAGIC *)
         | 0x4244     -> HFS_SUPER_MAGIC
         | 0xF995E849 -> HPFS_SUPER_MAGIC
         | 0x958458f6 -> HUGETLBFS_MAGIC
         | 0x9660     -> ISOFS_SUPER_MAGIC
         | 0x72b6     -> JFFS2_SUPER_MAGIC
         | 0x3153464a -> JFS_SUPER_MAGIC
         | 0x137F     -> MINIX_SUPER_MAGIC
         | 0x138F     -> MINIX_SUPER_MAGIC2
         | 0x2468     -> MINIX2_SUPER_MAGIC
         | 0x2478     -> MINIX2_SUPER_MAGIC2
         | 0x4d44     -> MSDOS_SUPER_MAGIC
         | 0x564c     -> NCP_SUPER_MAGIC
         | 0x6969     -> NFS_SUPER_MAGIC
         | 0x5346544e -> NTFS_SB_MAGIC
         | magic      -> UNKNOWN_SUPER_MAGIC magic
         end;
      f_bsize   = raw.Raw.f_bsize;
      f_blocks  = raw.Raw.f_blocks;
      f_bfree   = raw.Raw.f_bfree;
      f_bavail  = raw.Raw.f_bavail;
      f_files   = raw.Raw.f_files;
      f_ffree   = raw.Raw.f_ffree;
      f_namelen = raw.Raw.f_namelen
    }
  ;;
  external linux_statfs_stub : string -> Raw.t = "linux_statfs_stub" ;;
end ;;

let statfs path = Statfs.of_rawstatfs (Statfs.linux_statfs_stub path) ;;
