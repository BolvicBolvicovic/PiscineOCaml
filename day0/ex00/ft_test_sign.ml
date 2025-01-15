let ft_test_sign x = if x >= 0 then print_endline "positive" else print_endline "negative"

(* TESTS *)

let () =
        ft_test_sign 8;
        ft_test_sign (-8);
        ft_test_sign 0;
