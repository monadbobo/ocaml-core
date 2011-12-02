(* An abelian group

   To avoid confusing the mathematicians, an implementation of this interface should have
   the following properties:
   i) associativity: (a+b)+c = a+(b+c) for all elt's a,b,c
   ii) identity: zero+a = a+zero = a for all elt's a
   iii) inverses: given any elt a there exists a (unique) elt b such that a+b=b+a=zero
   Note closure is enforced.  Properties (i)-(iii) aren't explicitly enforced.

   Additionally, Ron indicated we probably want
   iv) commutativity: a+b=b+a
*)


module type S = sig
  type elt with sexp  (* an element of the group *)

  val zero : elt
  val (+)  : elt -> elt -> elt
  val (-)  : elt -> elt -> elt
end
