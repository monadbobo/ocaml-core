module type T = sig type t end

module type T_bin = sig type t with bin_io end

module type T1 = sig type 'a t end

module T1_poly = struct type 'a t = 'a end

module type T2 = sig type ('a, 'b) t end
