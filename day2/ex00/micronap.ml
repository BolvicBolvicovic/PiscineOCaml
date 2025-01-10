(* To include Unix module*)
(* with utop: utop -require unix micronap.ml [arg] *)
(* with ocamlotp: ocamlotp unix.cmxa micronap.ml *)

let my_sleep () = Unix.sleep 1
let () =
        if Array.length Sys.argv <> 2 then invalid_arg "Program should have one argument"
        else if int_of_string (Array.get Sys.argv 1) < 0 then invalid_arg "Argument should be positive, cannot wait negative seconds"
        else for i = 1 to int_of_string (Array.get Sys.argv 1) do my_sleep () done
