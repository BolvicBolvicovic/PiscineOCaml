type radar = float array * string
module StringMap = Map.Make(String)

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

let k_nn radars k radar =
        let nearests = Array.make k ("", Float.max_float) in
        let compare_nearests (name0, dist0) (name1, dist1) = Float.compare dist0 dist1 in
        let update_nearests (list, t) =
                let dist = eu_dist list (fst radar) in
                if dist < snd nearests.(k - 1) then (
                        nearests.(k - 1) <- (t, dist);
                        Array.sort compare_nearests nearests
                )
        in
        List.iter update_nearests radars;
        let new_nearests : int StringMap.t ref = ref StringMap.empty in
        let update_map (name, _) = 
                new_nearests := StringMap.update name ( function
                        | None -> Some 1
                        | Some count -> Some (count + 1)
                ) !new_nearests;
        in
        Array.iter update_map nearests;
        StringMap.fold (fun name count (best_name, best_count) ->
                if count > best_count then (name, count)
                else (best_name, best_count)) !new_nearests ("", 0)
