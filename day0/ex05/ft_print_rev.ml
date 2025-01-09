let ft_print_rev str =
        let str_i = String.length str - 1 in
        let rec loop x y =
                if y < 0 then
                        print_char '\n'
                else begin
                        print_char (String.get x y);
                        loop x (y - 1)
                end
        in
        loop str str_i;;

(* TEST *)
ft_print_rev "tester du 86";;
