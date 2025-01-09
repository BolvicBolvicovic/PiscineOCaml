let print_num x y z =
        print_int x;
        print_int y;
        print_int z;
        if x = 7 && y = 8 && z = 9 then
                ()
        else print_string ", ";;

let ft_print_comb () = 
        let rec loop x y z =
                if x = 8 then begin
                        print_string "\n";
                        ()
                end else if y = 9 then begin
                        if z <> 10 then print_num x y z;
                        loop (x + 1) (x + 2) (x + 3)
                end else if z = 9 then begin
                        print_num x y z;
                        loop x (y + 1) (y + 2)
                end else begin
                        print_num x y z;
                        loop x y (z + 1)
                end
        in
        loop 0 1 2;;

(* TEST *)
ft_print_comb ();;
