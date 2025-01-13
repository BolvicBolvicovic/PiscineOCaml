(* THIS IS THE TEST FILE FOR TRY MODULE *)
(*
        A monad is similar to a monoid except that f a -> Ma with M being extra data (metadata)
        
        For an expression;
        ( >>= ) = bind
        ( \a = lambda a)
        g : -> a -> Mb
        f : -> b -> Mc
        \a -> (g a) >>= \b -> (f b)
        \a -> [ Mb -> (b -> Mc) -> Mc ]
*)

let print_try_str t = 
        print_endline ( match t with
                | Exceptions.Try.Success t -> "Success " ^ t
                | Exceptions.Try.Failure e -> "Failure " ^ Printexc.to_string e
        

let new_print_endline t =
        print_endline t;
        Exceptions.Try.return t

let () =
        let my_str = "Hello Devin!" in
        let my_monad = Exceptions.Try.return my_str in
        let a_filter t_str = String.contains t_str 'a' in
        let my_filtered_monad = Exceptions.Try.filter my_monad a_filter in
        let my_super_monad = Exceptions.Try.return my_monad in
        let my_flattened_super_monad = Exceptions.Try.flatten my_super_monad in
        print_endline "PRINTING MONAD";
        print_try_str my_monad;
        print_try_str my_filtered_monad;
        print_try_str my_flattened_super_monad;
        print_endline "PRINTING STRING IF SUCCESS";
        let _ = Exceptions.Try.bind my_monad new_print_endline in
        let _ = Exceptions.Try.bind my_filtered_monad new_print_endline in
        let _ = Exceptions.Try.bind my_flattened_super_monad new_print_endline in
        ()
