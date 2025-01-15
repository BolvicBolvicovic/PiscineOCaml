let ft_is_palindrome str =
        let str_len = String.length str - 1 in
        if str_len = -1 then
                true
        else
                let rec loop i =
                        if i > str_len / 2 then
                                true
                        else if String.get str i <> String.get str (str_len - i) then
                                false
                        else
                                loop (i + 1)
                in
                loop 0

(* TESTS *)
let () = 
        assert (ft_is_palindrome "abba" = true);
        assert (ft_is_palindrome "abtba" = true);
        assert (ft_is_palindrome "abtbaa" = false);
        print_endline "ALL TESTS PASS";
