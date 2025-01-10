let eu_dist_part_1 f1 f2 = (f1 -. f2) ** 2.

let eu_dist a1 a2 =
        if Array.length a1 <> Array.length a2 then invalid_arg "Both arrays should have the same length";
        let combined_array = Array.map2 eu_dist_part_1 a1 a2 in
        let rec combined_float x y =
                if x >= Array.length a1 then y
                else combined_float (x + 1) (y +. Array.get combined_array x)
        in
        sqrt (combined_float 0 0.)


(* TESTS *)
let () =
        let a1 = [|42.;24.;|] in
        let a2 = [|24.;42.;|] in
        print_endline (string_of_float (eu_dist a1 a2))
