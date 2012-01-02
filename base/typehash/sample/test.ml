type 'a t = Foo of 'a * int * [`foo] with typehash
type u = int t with typehash

let result = Typehash.to_int typehash_of_u

module A = struct
  type t = string with typehash
end

module B = struct
  type t = string with typehash
end

module C = struct
  type t = string with typehash
end

type vartype =
| AA of A.t
| BB of B.t
| CC of C.t
with typehash

type vartype2 =
| D
| E of unit
| F of string
with typehash

type polyvartype = [
| `ABC
| `DEF of unit
| `GHI of A.t
| `JKL of B.t
] with typehash

type rectype = {
  a: A.t;
  b: B.t;
  c: C.t;
} with typehash
