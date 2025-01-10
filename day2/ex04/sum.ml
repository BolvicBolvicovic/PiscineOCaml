let sum f1 f2 = f1 +. f2


(* TESTS *)

let () =
        assert (sum 1. 5. = 6.);
        assert (sum 1. (-5.) = (-4.));
        print_endline "ALL TESTS PASS"
