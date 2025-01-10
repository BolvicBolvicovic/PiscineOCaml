let rec read_lines channel =
        try
                let line = input_line channel in
                line :: read_lines channel
        with End_of_file -> []

let get_jokes file_name =
        let channel = open_in file_name in
        let buffer =
                try
                        read_lines channel
                with e ->
                        close_in_noerr channel;
                        raise e
        in
        close_in channel;
        Array.of_list buffer


let random_jokes file_name =
        Random.self_init ();

        let jokes = get_jokes file_name in
        print_endline (Array.get jokes (Random.int (Array.length jokes)))

let () =
        if Array.length Sys.argv <> 2 then invalid_arg "Program requires a single filename as parameter"
        else if Sys.file_exists (Array.get Sys.argv 1) = false then invalid_arg "Filename does not refer to any file" 
        else random_jokes (Array.get Sys.argv 1)
