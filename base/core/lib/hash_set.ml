module Hashtbl = Core_hashtbl
let _hash = Core_hashtbl.hash           (* causes omake to work *shrug* *)
module List = StdLabels.List
open Sexplib
open Sexplib.Conv

type 'a t = ('a, unit) Hashtbl.t
type 'a hash_set = 'a t

let clear = Hashtbl.clear
let length = Hashtbl.length
let mem = Hashtbl.mem
let is_empty t = Hashtbl.length t = 0

let add t k = Hashtbl.replace t ~key:k ~data:()

exception Element_already_exists with sexp

let strict_add t k =
  if mem t k then Result.Error Element_already_exists
  else begin
    Hashtbl.replace t ~key:k ~data:();
    Result.Ok ()
  end

let strict_add_exn t k =
  Result.ok_exn (strict_add t k)

let remove = Hashtbl.remove

exception Element_not_in_set with sexp

let strict_remove t k =
  if mem t k then begin
    remove t k;
    Result.Ok ()
  end else Result.Error Element_not_in_set

let strict_remove_exn t k =
  Result.ok_exn (strict_remove t k)

let fold t ~init ~f = Hashtbl.fold t ~init ~f:(fun ~key ~data:() acc -> f acc key)
let iter t ~f = Hashtbl.iter t ~f:(fun ~key ~data:() -> f key)
let to_list = Hashtbl.keys

let exists t ~f = Hashtbl.existsi t ~f:(fun ~key ~data:() -> f key)
let for_all t ~f = not (Hashtbl.existsi t ~f:(fun ~key ~data:() -> not (f key)))

let equal t1 t2 = Hashtbl.equal t1 t2 (fun () () -> true)

let copy t = Hashtbl.copy t

let filter t ~f = Hashtbl.filteri t ~f:(fun ~key ~data:() -> f key)

let diff t1 t2 = filter t1 ~f:(fun key -> not (Hashtbl.mem t2 key))

let filter_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ac x ->
      if f x then ac else x :: ac)
  in
  List.iter to_remove ~f:(fun x -> remove t x)

let t_of_sexp_internal create k_of_sexp sexp =
  let t = create () in
  let keys = list_of_sexp k_of_sexp sexp in
  try
    List.iter keys ~f:(fun k -> strict_add_exn t k);
    t
  with
  | _ ->
    Conv.of_sexp_error
      (Printf.sprintf "Duplicate key in hash set: %s" (Sexp.to_string_hum sexp)) sexp

let of_list_internal create l =
  let t = create ~size:(List.length l) () in
  List.iter l ~f:(fun k -> add t k);
  t

let of_hashtbl_keys hashtbl = Hashtbl.map hashtbl ~f:ignore

module Poly = struct

  type 'a t = 'a hash_set

  let create = Hashtbl.Poly.create

  let of_list l = of_list_internal (fun ~size () -> create ~size ()) l

  let sexp_of_t sexp_of_k t = sexp_of_list sexp_of_k (to_list t)

  let t_of_sexp k_of_sexp sexp = t_of_sexp_internal create k_of_sexp sexp

end

module Make (H : Hashtbl.Key) = struct
  module T = Hashtbl.Make (H)

  type elem = H.t

  type 'a z = 'a t
  type t = H.t z

  let create = T.create

  let of_list l = of_list_internal (fun ~size () -> create ~size ()) l

  let t_of_sexp sexp = t_of_sexp_internal T.create H.t_of_sexp sexp

  let sexp_of_t set = Poly.sexp_of_t H.sexp_of_t set

end

module Make_binable (H : sig
  include Hashtbl.Key
  include Binable.S with type t := t
end) = struct
  include Make (H)

  type hash_set = t

  module Make_iterable_binable_spec = struct
    type t = hash_set
    type el = H.t with bin_io
    type acc = t
    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter
    let init size = create ~size ()
    let insert acc v _i = add acc v; acc
    let finish t = t
  end

  include Bin_prot.Utils.Make_iterable_binable (Make_iterable_binable_spec)
end
