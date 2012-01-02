(* This library serves a single purpose:

   To stop the following kind of code from compiling:

   (* 1st def*)
   type t = int with hashtype

   module M = struct
     (* 2nd def *)
     type t = bool
   end

   open M

   (* Point A *)
   type u = t with hashtype


   We need to make sure that the hashs defined at point A use values
   corresponding to the second definition of t and not the first.
*)

type 'a t = private int
external to_int : _ t -> int = "%identity"

(**/**)
type +'a internal
