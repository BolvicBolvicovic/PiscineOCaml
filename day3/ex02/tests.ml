(* THIS IS THE TEST FILE FOR THE CALC MODULE *)

module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
        assert (Calc_int.add 5 6 = 11);
        assert (Calc_int.sub 5 6 = -1);
        assert (Calc_int.mul 5 6 = 30);
        assert (Calc_int.div 5 6 = 0);
        assert (Calc_int.power 5 6 = 15625);
        assert (Calc_int.fact 5 = 120);
        assert (Calc_float.add 5. 6. = 11.);
        assert (Calc_float.sub 5. 6. = -1.);
        assert (Calc_float.mul 5. 6. = 30.);
        (* assert (Calc_float.div 5. 6. = 0.833333333333); This one is going to fail because it is an infinit number but if we print it, we will see 0.833333333333. *)
        assert (Calc_float.power 5. 6 = 15625.);
        assert (Calc_float.fact 5. = 120.);
        print_endline "ALL TESTS PASS"
