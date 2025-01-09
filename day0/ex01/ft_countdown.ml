let rec ft_countdown x = 
        if x > 0 then begin
                print_int x;
                print_char '\n'; 
                ft_countdown (x - 1);
        end else begin
                print_int 0;
                print_char '\n';
        end;;


(* TESTS *)
ft_countdown 8;;
ft_countdown (-7);;
ft_countdown 0;;
