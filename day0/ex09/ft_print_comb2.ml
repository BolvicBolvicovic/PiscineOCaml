let print_num x y =
        if x < 10 then print_int 0;
        print_int x;
        print_char ' ';
        if y < 10 then print_int 0;
        print_int y;
        if x = 98 && y = 99 then print_char '\n' else begin print_char ','; print_char ' ' end;;

let ft_print_comb2 () =
        let rec loop x y =
        if x = 99 then
                ()
        else if y = 99 then begin
                print_num x y;
                loop (x + 1) (x + 2)
        end else begin
                print_num x y;
                loop x (y + 1)
        end
        in
        loop 0 1;;

(* TEST *)
ft_print_comb2 ();;
