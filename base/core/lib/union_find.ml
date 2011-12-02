(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

include struct
  module List = Core_list
  let phys_equal = (==)
end

type 'a root = {
  mutable value: 'a;
  mutable rank: int;
}

type 'a t = { mutable parent : 'a parent; }
and 'a parent =
  | Parent of 'a t
  | Root of 'a root

let create v = { parent = Root { value = v; rank = 0; }; }

let compress t =
  let rec loop t ac =
    match t.parent with
    | Root _ ->
      let p = Parent t in
      List.iter ac ~f:(fun t -> t.parent <- p);
    | Parent t' -> loop t' (t :: ac)
  in
  loop t []

let representative t =
  compress t;
  match t.parent with
  | Root r -> (t, r)
  | Parent t ->
    match t.parent with
    | Root r -> (t, r)
    | Parent _ -> assert false

let root t = snd (representative t)

let get t = (root t).value

let set t v = (root t).value <- v

let same_class t1 t2 = phys_equal (root t1) (root t2)

let union t1 t2 =
  let (t1, r1) = representative t1 in
  let (t2, r2) = representative t2 in
  if phys_equal r1 r2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      t1.parent <- Parent t2
    else begin
      t2.parent <- Parent t1;
      if n1 = n2 then r1.rank <- r1.rank + 1;
    end

