open Std_internal

module Array = Core_array

module Entry = struct
  module T = struct
    type ('key, 'data) t =
      { key : 'key;
        mutable data : 'data;
        (* The index in [defined_entries] where this [Entry.t] is placed. *)
        mutable defined_entries_index : int;
      }
    with fields, sexp_of
  end
  include T
end

open Entry.T

type ('key, 'data) t =
  { num_keys : int;
    key_to_int : 'key -> int;
    (* The number of entries in the table, not the length of the arrays below. *)
    mutable length : int;
    (* (key, data) is in the table
       iff [entries_by_key.(key_to_index key) = { key; data ; _ }] *)
    entries_by_key : ('key, 'data) Entry.t option array;
    (* The first [length] elements of [defined_entries] hold the data in the table.
       This is an optimization for fold, to keep us from wasting iterations when
       the array is sparse. *)
    defined_entries : ('key, 'data) Entry.t option array;
  }
with fields, sexp_of

let to_sexp_ignore_data t =
  sexp_of_t
    (fun key -> Int.sexp_of_t (t.key_to_int key))
    (fun _ -> Sexp.Atom "<DATA>")
    t
;;

let invariant t =
  try
    let num_keys = t.num_keys in
    assert (num_keys = Array.length t.entries_by_key);
    assert (num_keys = Array.length t.defined_entries);
    assert (0 <= t.length && t.length <= num_keys);
    Array.iteri t.entries_by_key ~f:(fun i -> function
      | None -> ()
      | Some entry ->
        assert (i = t.key_to_int entry.key);
        match t.defined_entries.(entry.defined_entries_index) with
        | None -> assert false
        | Some entry' -> assert (phys_equal entry entry'));
    Array.iteri t.defined_entries ~f:(fun i entry_opt ->
      match i < t.length, entry_opt with
      | false, None -> ()
      | true, Some entry -> assert (i = entry.defined_entries_index)
      | _ -> assert false);
    let get_entries array =
      let a = Array.filter_opt array in
      Array.sort a ~cmp:(fun entry entry' ->
        Int.compare (t.key_to_int entry.key) (t.key_to_int entry'.key));
      a
    in
    let entries = get_entries t.entries_by_key in
    let entries' = get_entries t.defined_entries in
    assert (t.length = Array.length entries);
    assert (Array.equal entries entries' ~equal:phys_equal)
  with exn ->
    Error.raise (Error.arg "invariant failed"
                 <:sexp_of< exn * Sexp.t >>
                   (exn, to_sexp_ignore_data t))
;;

let debug = ref false

let check_invariant t = if !debug then invariant t

let create ~num_keys ~key_to_int =
  if num_keys < 0 then Error.raise (Error.arg "num_keys must be nonnegative"
                                    <:sexp_of< int >> num_keys);
  let t =
    { num_keys;
      key_to_int;
      length = 0;
      entries_by_key  = Array.create num_keys None;
      defined_entries = Array.create num_keys None;
    }
  in
  check_invariant t;
  t
;;

let fold t ~init ~f =
  let rec loop i ac =
    if i = t.length then
      ac
    else begin
      match t.defined_entries.(i) with
      | None -> assert false
      | Some entry -> loop (i + 1) (f ~key:entry.key ~data:entry.data ac)
    end
  in
  loop 0 init
;;

let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data)

let map_entries t ~f = fold t ~init:[] ~f:(fun ~key ~data ac -> f ~key ~data :: ac)

let to_alist t = map_entries t ~f:(fun ~key ~data -> (key, data))

type ('key, 'data) repr = ('key * 'data) list with sexp

let sexp_of_t sexp_of_key sexp_of_data t =
  sexp_of_repr sexp_of_key sexp_of_data (to_alist t)
;;

let keys t = map_entries t ~f:(fun ~key ~data:_ -> key)

let data t = map_entries t ~f:(fun ~key:_ ~data -> data)

let entry_opt t key =
  let index = t.key_to_int key in
  try t.entries_by_key.(index)
  with _ ->
    Error.raise (Error.arg "key out of range"
                 <:sexp_of< int * [ `Should_be_between_0_and of int ] >>
                   (index, `Should_be_between_0_and (t.num_keys - 1)))
;;

let find t key = Option.map (entry_opt t key) ~f:Entry.data

let mem t key = is_some (entry_opt t key)

let replace t ~key ~data =
  begin match entry_opt t key with
  | Some entry ->
    entry.data <- data;
  | None ->
    let defined_entries_index = t.length in
    let entry_opt = Some { Entry. key; data; defined_entries_index } in
    t.entries_by_key.(t.key_to_int key) <- entry_opt;
    t.defined_entries.(defined_entries_index) <- entry_opt;
    t.length <- t.length + 1;
  end;
  check_invariant t;
;;

let remove t key =
  begin match entry_opt t key with
  | None -> ()
  | Some entry ->
    t.length <- t.length - 1;
    t.entries_by_key.(t.key_to_int key) <- None;
    let hole = entry.defined_entries_index in
    let last = t.length in
    if hole < last then begin
      match t.defined_entries.(last) with
      | None -> assert false
      | Some entry_to_put_in_hole as entry_to_put_in_hole_opt ->
        t.defined_entries.(hole) <- entry_to_put_in_hole_opt;
        entry_to_put_in_hole.defined_entries_index <- hole;
    end;
    t.defined_entries.(last) <- None;
  end;
  check_invariant t;
;;

