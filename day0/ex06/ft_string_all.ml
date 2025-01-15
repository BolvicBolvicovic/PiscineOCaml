let predicate_char_is_a char = char = 'a'

let ft_string_all predicate str =
        let str_len = String.length str in
        let rec loop i =
                if i < str_len then
                        if predicate (String.get str i) = false then
                                false
                        else loop (i + 1)
                else
                        true
        in
        loop 0

(* TESTS *)
let () =
        assert (ft_string_all predicate_char_is_a "aaaaaaaaaaaaa" = true);
        assert (ft_string_all predicate_char_is_a "aaaaaadaaaa" = false);
        print_endline "ALL TESTS PASS";
