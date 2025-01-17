type radar = float array * string

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
