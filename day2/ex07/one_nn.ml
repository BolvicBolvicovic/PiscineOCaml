type radar = float array * string


let eu_dist_part_1 f1 f2 = (f1 -. f2) ** 2.

let eu_dist a1 a2 =
        if Array.length a1 <> Array.length a2 then invalid_arg "Both arrays should have the same length";
        let combined_array = Array.map2 eu_dist_part_1 a1 a2 in
        let rec combined_float x y =
                if x >= Array.length a1 then y
                else combined_float (x + 1) (y +. Array.get combined_array x)
        in
        sqrt (combined_float 0 0.)

let parse_line split_line =
        let t = ref ""  in
        let rec parse sl =
                try
                        let num = float_of_string (List.hd sl) in
                        num :: parse (List.tl sl)
                with _ ->
                        t := List.hd sl;
                        []
        in
        let parsed_line = parse split_line in
        (Array.of_list parsed_line, !t)
        
let rec read_lines channel =
        try
                let line = input_line channel in
                let parsed_line = parse_line (String.split_on_char ',' line) in
                parsed_line :: read_lines channel
        with End_of_file -> []

let examples_of_file file_path =
        let channel = open_in file_path in
        let examples = try
                read_lines channel
        with e ->
                close_in_noerr channel;
                raise e
        in
        close_in channel;
        examples

let one_nn radars radar =
        let nearest = ref ([||], "") in
        let nearest_eu = ref 0. in
        let update_nearest n =
                let dist = eu_dist (fst !n) (fst radar) in
                if !nearest_eu > dist then begin 
                        nearest := !n;
                        nearest_eu := dist
                end
        in
        List.iter update_nearest radars;
        snd !nearest
