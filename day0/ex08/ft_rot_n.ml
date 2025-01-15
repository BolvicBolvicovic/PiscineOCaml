let is_lowercase c = let q = int_of_char c in q >= 97 && q <= 122
let is_uppercase c = let q = int_of_char c in q >= 65 && q <= 90

let ft_rot_n n code =
        let rot_c c =
                if is_lowercase c then
                        char_of_int ((int_of_char c - 97 + n) mod 26 + 97)
                else if is_uppercase c then
                        char_of_int ((int_of_char c - 65 + n) mod 26 + 65)
                else
                        c
        in
        String.map rot_c code

(* TESTS *)
let () =
        assert (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" = "bcdefghijklmnopqrstuvwxyza");
        assert (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" = "nopqrstuvwxyzabcdefghijklm");
        assert (ft_rot_n 42 "0123456789" = "0123456789");
        assert (ft_rot_n 2 "OI2EAS67B9" = "QK2GCU67D9");
        assert (ft_rot_n 0 "Damned !" = "Damned !");
        assert (ft_rot_n 42 "" = "");
        assert (ft_rot_n 1 "NBzlk qnbjr !" = "OCaml rocks !");
        print_endline "ALL TESTS PASS";
