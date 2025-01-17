module type PAIR = sig
        val pair : (int * int)
end

module type VAL = sig
        val x : int
end


module MakeFst : functor (P: PAIR) -> VAL = functor (P: PAIR) -> struct
        let x = fst P.pair
end

module MakeSnd : functor (P : PAIR ) -> VAL = functor (P: PAIR) -> struct
        let x = snd P.pair
end

module Pair : PAIR = struct let pair = ( 21, 42 ) end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
