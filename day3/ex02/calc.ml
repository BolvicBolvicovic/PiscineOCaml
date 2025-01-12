module type MONOID = sig
        type element
        val zero1 : element
        val zero2 : element
        val mul : element -> element -> element
        val add : element -> element -> element
        val div : element -> element -> element
        val sub : element -> element -> element
end

module INT : MONOID with type element = int = struct
        type element = int
        let zero1 = 0
        let zero2 = 1
        let mul x y = x * y
        let add x y = x + y
        let div x y = x / y
        let sub x y = x - y
end

module FLOAT : MONOID with type element = float = struct
        type element = float
        let zero1 = 0.
        let zero2 = 1.
        let mul x y = x *. y
        let add x y = x +. y
        let div x y = x /. y
        let sub x y = x -. y
end

module type CALC = functor (M: MONOID) -> sig
        val add : M.element -> M.element -> M.element
        val sub : M.element -> M.element -> M.element
        val mul : M.element -> M.element -> M.element
        val div : M.element -> M.element -> M.element
        val power : M.element -> int -> M.element
        val fact : M.element -> M.element
end

module Calc : CALC = functor (M: MONOID) -> struct
        let add x y = M.add x y
        let sub x y = M.sub x y
        let mul x y = M.mul x y
        let div x y = M.div x y
        let rec power x e = if e = 0 then M.zero2 else if e = 1 then x else M.mul x (power x (e - 1))
        let rec fact x = if x = M.zero1 then M.zero2 else M.mul x (fact (M.sub x M.zero2))
end
