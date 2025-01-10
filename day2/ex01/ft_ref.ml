type 'a ft_ref = {
        mutable contents : 'a;
}

let return a = { contents = a }
let get a = a.contents
let set a b = a.contents <- b
let bind a b = a.contents <- b a.contents

(* TESTS *)

let test_bind a = a + 60

let () =
        let a1 = 5 in
        let a2 = 6 in
        let ra1 = return a1 in
        assert (get ra1 = 5);
        set ra1 a2;
        assert (get ra1 = 6);
        bind ra1 test_bind;
        assert (get ra1 = 66);
        print_endline "ALL TESTS PASS";;
