(* TESTS FILE FOR WATCHTOWER MONOIDS *)
(* A monoids is a collection of things plus a rule to combine them.
   This rule has to respect some basic rules:
        - assosiativity: x combine with (y combine with z) = (x combine with y) combine with z
        - has to have a special member known a unit or zero (sm) as x combine with sm = x and sm combine with x = x
*)

let () =
        let x = 42 in
        let y = 6 in
        assert ( Watchtower.add x y = 0 );
        assert ( Watchtower.sub x y = 0 );
        print_endline "ALL TESTS PASS"
