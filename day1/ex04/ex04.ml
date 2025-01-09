module type VAL = sig
        type t
        val add : t -> t -> t
        val mul : t -> t -> t
end

module type EVALEXPR = sig
        type t
        type expr =
                | Literal of t
                | Sum of expr * expr
                | Product of expr * expr
        val eval : expr -> t
end

module type MAKEEVALEXPR = functor (V: VAL) -> EVALEXPR 

module MakeEvalExpr : MAKEEVALEXPR = functor (V: VAL) -> struct
        type t = V.t
        type expr = 
                | Literal of t
                | Sum of expr * expr
                | Product of expr * expr
        let rec eval = function
                | Literal t -> t
                | Sum (e1, e2) -> V.add (eval e1) (eval e2)
                | Product (e1, e2) -> V.mul (eval e1) (eval e2)
end
