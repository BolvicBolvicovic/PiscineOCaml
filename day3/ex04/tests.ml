(* THIS IS THE TEST FILE FOR THE SET MODULE *)
open Set

let print_for_each_str element =
        print_endline element

let print_bind_set_str element =
        print_endline element;
        Set.return element

let () =
        let my_set = Set.return "Hello" in
        let my_other_set = Set.return "World" in
        let my_union_set = Set.union my_set my_other_set in
        let my_inter_set = Set.inter my_set my_union_set in
        let my_diff_set = Set.diff my_set my_union_set in
        let my_filter_set = Set.filter my_union_set (fun e -> e = "Hello") in
        let my_bool_for_all = Set.for_all my_union_set (fun e -> e = "Hello") in
        let my_bool_exists = Set.exists my_union_set (fun e -> e = "Hello") in

        (* Printing twice, once with bind, the other with foreach *)
        print_endline "Bind tests :";
        let my_set_bind = Set.bind my_set print_bind_set_str in
        let my_other_set_bind = Set.bind my_other_set print_bind_set_str in
        let my_union_set_bind = Set.bind my_union_set print_bind_set_str in

        print_endline "Foreach tests :";
        Set.foreach my_set_bind print_for_each_str;
        Set.foreach my_other_set_bind print_for_each_str;
        Set.foreach my_union_set_bind print_for_each_str;

        assert (my_inter_set = my_set);
        assert (my_diff_set = my_other_set);
        assert (my_filter_set = my_set);
        assert (my_bool_for_all = false);
        assert (my_bool_exists = true);



        print_endline "ALL TESTS PASS"
