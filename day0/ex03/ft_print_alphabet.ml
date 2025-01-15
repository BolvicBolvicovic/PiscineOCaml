let ft_print_alphabet () =
        let rec loop x =
                if x > 122 then
                        ()
                else begin
                        print_char (char_of_int x);
                        loop (x + 1)
                end
        in
        loop 97;
        print_char '\n'

(* TEST *)
let () =
        ft_print_alphabet ();
