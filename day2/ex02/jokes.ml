let () =
        Random.self_init ();

        let jokes = [|
                "Why don't skeletons fight each other? They don't have the guts.";
                "Why did the scarecrow win an award? Because he was outstanding in his field.";
                "Why did the golfer bring two pairs of pants? In case he got a hole in one.";
                "What do you call cheese that isn’t yours? Nacho cheese.";
                "What did the ocean say to the beach? Nothing, it just waved.";
        |]
        in
        print_endline (Array.get jokes (Random.int 5))
