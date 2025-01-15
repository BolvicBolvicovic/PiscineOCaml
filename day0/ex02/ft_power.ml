let rec rec_power x y z = if y = 0 then z else rec_power x (y - 1) (z * x)

let ft_power x y = 
        if x = 0 then
                0
        else if y = 0 then
                1
        else if y = 1 then
                x
        else 
                rec_power x y 1

(* TEST *)
let () =
        assert (ft_power 2 4 = 16);
        assert (ft_power 3 0 = 1);
        assert (ft_power 0 5 = 0);
        print_endline "TEST SUCCEDED!"
