module type TRY = sig
        type 'a t =
                | Success of 'a
                | Failure of exn
        val return : 'a -> 'a t
        val bind : 'a t -> ('a -> 'b t) -> 'b t
        val recover : 'a t -> (exn -> 'a t) -> 'a t
        val filter : 'a t -> ('a -> bool) -> 'a t
        val flatten : 'a t t -> 'a t
end

module Try : TRY = struct
        type 'a t =
                | Success of 'a
                | Failure of exn
        let return a = Success a
        let bind t f = match t with
                | Success t -> f t
                | Failure e -> Failure e
        let recover t f = match t with
                | Success t -> Success t
                | Failure e -> f e
        let filter t f = match t with
                | Success t -> begin match f t with
                        | true -> Success t
                        | false -> Failure (Invalid_argument "false")
                        end
                | Failure e -> Failure e
        let flatten t = match t with
                | Success t -> t
                | Failure e -> Failure e
end
