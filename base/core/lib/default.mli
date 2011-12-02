(* [Default] is used to create new types for specifying default values for optional
   arguments, and having the type checker enforce the default.  See

     http://ocaml.janestreet.com/?q=node/96
*)

type ('real, 'phantom) t = private 'real

val override : 'real -> ('real, _) t

module type S = sig
  type phantom
  type real
  type default = (real, phantom) t
  val default : default
end

val create : 'a -> (module S with type real = 'a)

