module type FRACTIONNAL_BITS = sig
        val bits : int
end

module type FIXED = sig
        type t
        val of_float : float -> t
        val of_int : int -> t
        val to_float : t -> float
        val to_int : t -> int
        val to_string : t -> string
        val zero : t
        val one : t
        val succ : t -> t
        val pred : t -> t
        val min : t -> t -> t
        val max : t -> t -> t
        val gth : t -> t -> bool
        val lth : t -> t -> bool
        val gte : t -> t -> bool
        val lte : t -> t -> bool
        val eqp : t -> t -> bool (** physical equality *)
        val eqs : t -> t -> bool (** structural equality *)
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE = functor (F: FRACTIONNAL_BITS) -> FIXED

module Make : MAKE = functor (F: FRACTIONNAL_BITS) -> struct
        type t = int
        let of_float f = int_of_float (Float.round (f *. (2. ** float_of_int F.bits)))
        let of_int i = i lsl F.bits
        let to_float n = float_of_int n /. (2. ** float_of_int F.bits)
        let to_int n = n lsr F.bits
        let to_string n = string_of_float (to_float n)
        let zero = 0
        let one = 1 lsl F.bits
        let succ n = n + 1
        let pred n = n - 1
        let min n m = if n <= m then n else m
        let max n m = if n >= m then n else m
        let gth n m = n > m
        let lth n m = n < m
        let gte n m = n >= m
        let lte n m = n <= m
        let eqp n m = n == m
        let eqs n m = n = m
        let add n m = n + m
        let sub n m = n - m
        let mul n m = n * m
        let div n m = n / m
        let foreach n m f = 
                let rec do_all n m =
                        if n > m then ()
                        else begin f n; do_all (succ n) m end
                in
                do_all n m
end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
        let x8 = Fixed8.of_float 21.10 in
        let y8 = Fixed8.of_float 21.32 in
        let a8 = Fixed8.add x8 y8 in
        let s8 = Fixed8.sub x8 y8 in
        let m8 = Fixed8.mul x8 y8 in
        let d8 = Fixed8.div x8 y8 in
        print_endline ("Addition x8 + y 8 " ^ Fixed8.to_string a8);
        print_endline ("Substraction x8 - y8 " ^ Fixed8.to_string s8);
        print_endline ("Multiplication x8 * y8 " ^ Fixed8.to_string m8);
        print_endline ("Division x8 / y8 " ^ Fixed8.to_string d8);
        assert (Fixed8.gth y8 x8 = true);
        assert (Fixed8.lth x8 y8 = true);
        assert (Fixed8.lte x8 y8 = true);
        assert (Fixed8.gte y8 x8 = true);
        assert (Fixed8.eqp x8 x8 = true);
        assert (Fixed8.eqs x8 x8 = true);

        Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));

