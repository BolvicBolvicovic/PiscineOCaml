let rec read_lines channel =
        try
                let line = input_line channel in
                line :: read_lines channel
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
