module M1 = struct type t = unit with compare end

module M2 = struct type t = int with compare end

module M3 = struct type t = bool with compare end

module M4 = struct type t = int32 with compare end

module M5 = struct type t = nativeint with compare end

module M6 = struct type t = int64 with compare end

module M7 = struct type t = float with compare end

module M8 = struct type t = bool * float with compare end

module M9 = struct type t = bool * float * int with compare end

module M10 = struct type t = bool * float * int * string with compare  end

module M11 = struct type t = int ref with compare end

module M12 = struct type t = (float * float) option with compare end

module M13 = struct type t = float array with compare end

module M14 = struct type t = (int * int) array with compare end

module M15 = struct type t = float array array with compare end

module M16 = struct type t = int list with compare end

module M17 = struct type t = {
  s : string;
  b : float array list;
  mutable c : (int * int64 option);
} with compare
end

module M18 = struct type t = {
  a : float;
  b : float;
  c : float;
} with compare
end

module M19 = struct type t = Foo with compare end

module M20 = struct type t = Foo of int with compare end

module M21 = struct type t = Foo of int * float with compare    end

module M22 = struct type t = Foo | Bar of int | Baz of string option with compare end

module M23 = struct type t = [`Foo | `Bar of string * string] with compare end

module M24 = struct type t = int * string * [`Foo | `Bar ] with compare end

module M25 = struct type t = String.t with compare end

module M26 = struct type 'a t = 'a array with compare end

module MyList = struct type 'a t = Nil | Node of 'a * 'a t with compare end
