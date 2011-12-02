open Sexplib

include Core_hashtbl_intf

(* We would like to include the key here, but since this is not
 a functor we don't know if it's sexpable or not. *)
exception Add_key_already_present

(* Copied from Inria hashtbl.ml *)
external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
let hash x = hash_param 10 100 x

(* A few small things copied from other parts of core because
   they depend on us, so we can't use them. *)
module Int = struct
  type t = int

  let max (x : t) y = if x > y then x else y
  let min (x : t) y = if x < y then x else y
end

module List = Core_list
module Array = Core_array

let phys_equal = (==)

let poly = { hash = hash; compare = compare }

module T = struct
  type ('k, 'v) t =
    { mutable table : ('k, 'v) Avltree.t array;
      mutable array_length: int;
      mutable length : int;
      growth_allowed: bool;
      added_or_removed : bool ref;
      hashable: 'k hashable;
    }
end

include T

let create ?(growth_allowed=true) ?(hashable = poly) ?(size = 128) () =
  let size = Int.min (Int.max 1 size) Sys.max_array_length in
  { table = Array.create size Avltree.empty;
    array_length = size;
    length = 0;
    growth_allowed = growth_allowed;
    added_or_removed = ref false;
    hashable = hashable };;

let slot t key = t.hashable.hash key mod t.array_length

let add_worker should_replace t ~key ~data =
  let i = slot t key in
  let root = t.table.(i) in
  let new_root =
    (* The avl tree might replace (Avltree.add) or do nothing
      (Avltree.add_if_not_exists) to the entry, in that case the table
      did not get bigger, so we should not increment length, we
      pass in the bool ref t.added so that it can tell us whether
      it added or replaced. We do it this way to avoid extra
      allocation. Since the bool is an immediate it does not go
      through the write barrier. *)
    (if should_replace then Avltree.add else Avltree.add_if_not_exists) root
      ~compare:t.hashable.compare ~added:t.added_or_removed ~key ~data
  in
  if t.added_or_removed.contents then
    t.length <- t.length + 1;
  (* This little optimization saves a caml_modify when the tree
    hasn't been rebalanced. *)
  if not (phys_equal new_root root) then
    t.table.(i) <- new_root
;;

let maybe_resize_table t =
  let should_grow = t.length >= t.array_length * 2 in
  if should_grow && t.growth_allowed then begin
    let new_array_length =
      Int.min (t.array_length * 2) Sys.max_array_length
    in
    if new_array_length > t.array_length then begin
      let new_table =
        Array.init new_array_length ~f:(fun _ -> Avltree.empty)
      in
      let old_table = t.table in
      t.array_length <- new_array_length;
      t.table <- new_table;
      t.length <- 0;
      for i = 0 to Array.length old_table - 1 do
        Avltree.iter old_table.(i) ~f:(fun ~key ~data ->
          add_worker true t ~key ~data)
      done
    end
  end
;;

let set t ~key ~data =
  add_worker true t ~key ~data;
  maybe_resize_table t
;;
let replace = set
;;

let add t ~key ~data =
  add_worker false t ~key ~data;
  if !(t.added_or_removed) then begin
    maybe_resize_table t;
    `Ok
  end else
    `Duplicate
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate -> raise Add_key_already_present
;;

let clear t =
  for i = 0 to t.array_length - 1 do
    t.table.(i) <- Avltree.empty;
  done;
  t.length <- 0
;;

let find t key =
  Avltree.find t.table.(slot t key) ~compare:t.hashable.compare key

let mem t key =
  Avltree.mem t.table.(slot t key) ~compare:t.hashable.compare key

let remove t key =
  let i = slot t key in
  let root = t.table.(i) in
  let new_root =
    Avltree.remove root
      ~removed:t.added_or_removed ~compare:t.hashable.compare key
  in
  if not (phys_equal root new_root) then
    t.table.(i) <- new_root;
  if t.added_or_removed.contents then
    t.length <- t.length - 1
;;

let length t = t.length

let is_empty t = length t = 0

let fold t ~init ~f =
  let n = t.array_length in
  let acc = ref init in
  for i = 0 to n - 1 do
    let init = !acc in
    acc := Avltree.fold t.table.(i) ~init ~f;
  done;
  !acc
;;

let invariant t =
  for i = 0 to t.array_length - 1 do
    Avltree.invariant t.table.(i) ~compare:t.hashable.compare
  done;
  assert (Array.length t.table = t.array_length);
  let real_len = fold t ~init:0 ~f:(fun ~key:_ ~data:_ i -> i + 1) in
  assert (real_len = t.length)
;;

let find_exn t id =
  match find t id with
  | None -> raise Not_found
  | Some x -> x

let find_default t key ~default =
  match find t key with
  | None -> default ()
  | Some a -> a

let iter t ~f =
  fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

let existsi t ~f =
  With_return.with_return (fun r ->
    iter t ~f:(fun ~key ~data -> if f ~key ~data then r.With_return.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data)

let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data -> replace new_t ~key ~data:(f ~key ~data));
  new_t

(* How about this? *)
(*
let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let itfun ~key ~data = replace new_t ~key ~data:(f ~key ~data) in
  iter t ~f:itfun;
  new_t
*)

let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)

let copy t = map t ~f:Fn.id

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> replace new_t ~key ~data:new_data
    | None -> ());
  new_t

(* How about this? *)
(*
let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let itfun ~key ~data = match f ~key ~data with
    | None -> ()
    | Some d -> replace new_t ~key ~data:d
  in
  iter t ~f:itfun;
  new_t
*)

let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data)

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None)
;;

let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data)

let partition_mapi t ~f =
  let t0 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let t1 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | `Fst new_data -> replace t0 ~key ~data:new_data
    | `Snd new_data -> replace t1 ~key ~data:new_data);
  (t0, t1)
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)

let partitioni t ~f =
  partition_mapi t ~f:(fun ~key ~data -> if f ~key ~data then `Fst data else `Snd data)
;;

let partition t ~f = partitioni t ~f:(fun ~key:_ ~data -> f data)

let remove_all = remove

let remove_one t key =
  match find t key with
  | None -> ()
  | Some ([] | [_]) -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl

let find_or_add t id ~default =
  match find t id with
  | Some x -> x
  | None ->
    let default = default () in
    replace t ~key:id ~data:default;
    default

let change t id f =
  match f (find t id) with
  | None -> remove t id
  | Some data -> replace t ~key:id ~data

let incr ?(by = 1) t key =
  change t key
    (function
      | None -> Some 1
      | Some i -> Some (i + by))

let add_multi t ~key ~data =
  match find t key with
  | None -> replace t ~key ~data:[data]
  | Some l -> replace t ~key ~data:(data :: l)

let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data)

let of_alist ?growth_allowed ?hashable ?size lst =
  let size = match size with Some s -> s | None -> List.length lst in
  let t = create ?growth_allowed ?hashable ~size () in
  let res = ref (`Ok t) in
  List.iter lst ~f:(fun (k, v) ->
    match mem t k with
    | true -> res := `Duplicate_key k
    | false -> replace t ~key:k ~data:v);
  !res
;;

let of_alist_report_all_dups ?growth_allowed ?hashable ?size lst =
  let size = match size with Some s -> s | None -> List.length lst in
  let t = create ?growth_allowed ?hashable ~size () in
  let dups = ref [] in
  List.iter lst ~f:(fun (k, v) ->
    match mem t k with
    | true -> dups := k::!dups
    | false -> replace t ~key:k ~data:v);
  match !dups with
  | []   -> `Ok t
  | keys -> `Duplicate_keys (List.dedup keys)
;;

let of_alist_exn ?growth_allowed ?hashable ?size lst =
  match of_alist ?growth_allowed ?hashable ?size lst with
  | `Ok v -> v
  | `Duplicate_key _k -> failwith "Hashtbl.of_alist_exn: duplicate key"
;;

let of_alist_multi ?growth_allowed ?hashable ?size lst =
  let lst = List.rev lst in
  let size = match size with Some s -> s | None -> List.length lst in
  let t = create ?growth_allowed ?hashable ~size () in
  List.iter lst ~f:(fun (key, data) ->
    add_multi t ~key ~data);
  t
;;

let to_alist t =
  fold ~f:(fun ~key ~data list -> (key, data)::list) ~init:[] t
;;

let keys t = fold t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

let data t = fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] t

let add_to_groups groups ~get_key ~get_data ~combine ~rows =
  List.iter rows ~f:(fun row ->
    let key = get_key row in
    let data = get_data row in
    let data =
      match find groups key with
      | None -> data
      | Some old -> combine old data
    in
    replace groups ~key ~data)
;;

let group ?growth_allowed ?hashable ?size ~get_key ~get_data ~combine rows =
  let res = create ?growth_allowed ?hashable ?size () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_mapped ?growth_allowed ?hashable ?size ~get_key ~get_data rows =
  let res = create ?growth_allowed ?hashable ?size () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    replace res ~key ~data);
  res
;;

let create_with_key ?growth_allowed ?hashable ?size ~get_key rows =
  create_mapped ?growth_allowed ?hashable ?size ~get_key ~get_data:(fun x -> x) rows
;;

let merge t1 t2 ~f =
  if not (phys_equal t1.hashable t2.hashable)
  then invalid_arg "Hashtbl.merge: different 'hashable' values";
  let create () =
    create
      ~growth_allowed:t1.growth_allowed
      ~hashable:t1.hashable
      ~size:t1.length
      ()
  in
  let t = create () in
  let unique_keys = create () in
  let record_key ~key ~data:_ = replace unique_keys ~key ~data:() in
  iter t1 ~f:record_key;
  iter t2 ~f:record_key;
  iter unique_keys ~f:(fun ~key ~data:_ ->
    let arg =
      match find t1 key, find t2 key with
      | None, None -> assert false
      | None, Some r -> `Right r
      | Some l, None -> `Left l
      | Some l, Some r -> `Both (l, r)
    in
    match f ~key arg with
    | Some data -> replace t ~key ~data
    | None -> ());
  t
;;

let merge_into ~f ~src ~dst =
  iter src ~f:(fun ~key ~data ->
    match f ~key data (find dst key) with
    | Some data -> replace dst ~key ~data
    | None -> ())

let filteri_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ~key ~data ac ->
      if f key data then ac else key :: ac)
  in
  List.iter to_remove ~f:(fun key -> remove t key);
;;

let filter_inplace t ~f =
  filteri_inplace t ~f:(fun _ data -> f data)
;;

module T_sexpable = struct

  let sexp_of_t sexp_of_k sexp_of_d t =
    let coll ~key:k ~data:v acc = Sexp.List [sexp_of_k k; sexp_of_d v] :: acc in
    Sexp.List (fold ~f:coll t ~init:[])
  ;;

  let t_of_sexp_internal create k_of_sexp d_of_sexp sexp =
    match sexp with
    | Sexp.List sexps ->
      let t = create () in
      List.iter sexps ~f:(function
        | Sexp.List [k_sexp; v_sexp] ->
          let key = k_of_sexp k_sexp in
          if mem t key then
            failwith
              (Printf.sprintf "Hashtbl.t_of_sexp: duplicate key %s"
                 (Sexp.to_string k_sexp))
          else
            replace t ~key ~data:(d_of_sexp v_sexp)
        | Sexp.List _ | Sexp.Atom _ ->
          Sexplib.Conv.of_sexp_error "Hashtbl.t_of_sexp: tuple list needed" sexp);
      t
    | Sexp.Atom _ ->
      Sexplib.Conv.of_sexp_error
        "Hashtbl.t_of_sexp: found atom where list was expected" sexp
  ;;

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    t_of_sexp_internal create k_of_sexp d_of_sexp sexp
  ;;
end

include T_sexpable

open With_return

let equal t t' equal =
  length t = length t' &&
  with_return (fun r ->
    iter t ~f:(fun ~key ~data ->
      match find t' key with
      | None -> r.return false
      | Some data' -> if not (equal data data') then r.return false);
    true)
;;

module Table_fns (Key : Key) = struct
  let invariant       = invariant
  let clear           = clear
  let copy            = copy
  let remove          = remove
  let remove_one      = remove_one
  let replace         = replace
  let set             = set
  let add             = add

  exception Add_exn_duplicate_key of Key.t with sexp
  let add_exn t ~key ~data =
    match add t ~key ~data with
    | `Ok -> ()
    | `Duplicate -> raise (Add_exn_duplicate_key key)
  ;;

  let change          = change
  let add_multi       = add_multi
  let mem             = mem
  let iter            = iter
  let exists          = exists
  let existsi         = existsi
  let fold            = fold
  let length          = length
  let is_empty        = is_empty
  let map             = map
  let mapi            = mapi
  let filter_map      = filter_map
  let filter_mapi     = filter_mapi
  let filter          = filter
  let filteri         = filteri
  let partition_map   = partition_map
  let partition_mapi  = partition_mapi
  let partition       = partition
  let partitioni      = partitioni
  let find_or_add     = find_or_add
  let find            = find
  let find_exn        = find_exn
  let iter_vals       = iter_vals
  let to_alist        = to_alist
  let merge           = merge
  let merge_into      = merge_into
  let keys            = keys
  let data            = data
  let filter_inplace  = filter_inplace
  let filteri_inplace = filteri_inplace
  let equal           = equal
  let add_to_groups   = add_to_groups
  let incr            = incr
end

module T_binable =
  Bin_prot.Utils.Make_iterable_binable2 (struct
    type ('a, 'b) z = ('a, 'b) t
    type ('a, 'b) t = ('a, 'b) z
    type ('a, 'b) el = 'a * 'b with bin_io
    type ('a, 'b) acc = ('a, 'b) t

    let module_name = Some "Core_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
    ;;

    let finish = Fn.id
  end)

include T_binable

module Create_fns (H : sig type 'a key val hashable : 'a key hashable end) = struct
  let hashable = H.hashable
  let create                   = create                   ~hashable
  let of_alist                 = of_alist                 ~hashable
  let of_alist_report_all_dups = of_alist_report_all_dups ~hashable
  let of_alist_exn             = of_alist_exn             ~hashable
  let of_alist_multi           = of_alist_multi           ~hashable
  let group                    = group                    ~hashable
  let create_mapped            = create_mapped            ~hashable
  let create_with_key          = create_with_key          ~hashable
end

module Poly = struct
  include T_binable
  include T_sexpable
  include Create_fns (struct type 'a key = 'a let hashable = poly end)
end

(* This has to be outside of Make because 'exception ... with sexp' doesn't work reliably
   in functors *)
exception Of_alist_exn_duplicate_key of Sexp.t with sexp

module Make (Key: Key) = struct
  let hashable = {
    hash = Key.hash;
    compare = Key.compare;
  }
  module Key = Key

  type 'a t = (Key.t, 'a) T.t
  let sexp_of_t sexp_of_d t = sexp_of_t Key.sexp_of_t sexp_of_d t

  include Create_fns (struct type 'a key = Key.t let hashable = hashable end)

  let of_alist_exn ?growth_allowed ?size lst =
    match of_alist ?growth_allowed ?size lst with
    | `Ok v -> v
    | `Duplicate_key key -> raise (Of_alist_exn_duplicate_key (Key.sexp_of_t key))
  ;;

  include Table_fns (Key)

  let t_of_sexp d_of_sexp sexp =
    t_of_sexp_internal create Key.t_of_sexp d_of_sexp sexp

end

module Make_binable (Key' : sig
  include Key
  include Binable.S with type t := t
end) = struct
  include Make (Key')

  module Make_iterable_binable1_spec = struct
    type 'a acc = 'a t
    type 'a t = 'a acc
    type 'a el = Key'.t * 'a with bin_io

    let module_name = Some "Core_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ ->
        failwith (Printf.sprintf "Core_hashtbl.bin_read_t_: duplicate key %s"
                    (Sexp.to_string (Key.sexp_of_t key)))
    ;;

    let finish = Fn.id
  end

  include Bin_prot.Utils.Make_iterable_binable1 (Make_iterable_binable1_spec)
end
