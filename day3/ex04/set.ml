module Set : sig 
        type 'a t
        val return  : 'a -> 'a t
        val bind    : 'a t -> ('a -> 'b t) -> 'b t
        val union   : 'a t -> 'a t -> 'a t
        val inter   : 'a t -> 'a t -> 'a t
        val diff    : 'a t -> 'a t -> 'a t
        val filter  : 'a t -> ('a -> bool) -> 'a t
        val foreach : 'a t -> ('a -> unit) -> unit
        val for_all : 'a t -> ('a -> bool) -> bool
        val exists  : 'a t -> ('a -> bool) -> bool
end = struct 
        type 'a t = 'a list

        let return a = [a]
        let bind set f = List.fold_left (fun acc element -> acc @ f element) [] set
        let union set1 set2 = List.fold_left (fun acc element -> acc @ [element]) set1 set2
        let inter set1 set2 = List.fold_left (fun acc element -> acc @ if List.exists (fun a -> a = element) set2 then [element] else []) [] set1
        let diff set1 set2 = 
                List.fold_left (fun acc element -> acc @ if List.exists (fun a -> a = element) set2 then [] else [element]) [] set1 @
                List.fold_left (fun acc element -> acc @ if List.exists (fun a -> a = element) set1 then [] else [element]) [] set2

        let filter set f = List.fold_left (fun acc element -> acc @ if f element then [element] else []) [] set
        let foreach set f = List.iter f set
        let for_all set f = List.for_all f set
        let exists set f = List.exists f set
end
