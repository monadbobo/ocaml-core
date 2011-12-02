Pa_compare is a camlp4 syntax extension that derives comparison functions from
type representations. The scaffolded functions are usually much faster than
ocaml's [Pervasives.compare] (cf. the programs in benchmark/). Scaffolding
functions also gives you more flexibilty by allowing you to override them
for a specific type and more safety by making sure that you only compare
comparable values.

We use the type_conv framework so:

  [ type s = v * w with compare ]

will generate and [compare_s : s -> s -> int ] function that relies on
[compare_v : v -> v -> int] and [compare_w : w -> w -> int].

 ______________________________________________________________________________
|  .                                                                           |
| /!\  Compare is not DWIM (do what I mean) : it will scaffold a fast          |
| ^^^  well behaved comparison (reflexive, transitive, symmetric...) function  |
| however it does not try to follow any "natural ordering". For instance       |
| arrays of characters are not sorted lexicographically.                       |
|______________________________________________________________________________|


Base types (options,int,array,lists,char,floats...) have the same comparison order than Pervasives.compare (providing their type parameters also do for the polymorphic ones).

Records fields are compared in the order they are defined (left to right); tuples fields are compared left to right. When we compare two branches of a sum whichever ones comes first in the definition is considered lowest. Variants use a fairly odd ordering (the same as the ocaml runtime)...

special features:


Calling [compare] for type [t]s
================================

In compliance (or conformance) with Janestreet's coding standard we assume that
type named [t] are the main types in a module and

  [ type t = S.t * T.t with compare ]

will call the functions [S.compare] and [T.compare] instead of calling
[S.compare_t] and [T.compare_t]. This will also generate a
[compare : t -> t -> int] function the [compare_t] function is also generated
for consistency's sake.

Signature
=========

  [type t with compare] in a module signature will add the
  [ compare : t -> t -> int ] function in the signature (please note that the
  functions [compare_t] is not exported...).

Quotation
==========

Sometimes you just want to compare values and not to create new types for
them. You can call the comparison function for a specific input?argument?using
quotations:

  [ let gt x y = <:compare< float * int * [`A | `B | `C] >> x y ]

Tasking compare for a quick spin
================================

The [preprocess.sh] script will show you the result of applying the syntax
extension to a file. If you have your type definition in a file [f.ml] you
can look  at the generated comparaison functions by calling [preprocess.sh f.ml]
