open Variantslib
type t =  Foo of int | Bar with variants
let print_all_variants =
  List.map print_endline (List.map fst Variants.descriptions)
