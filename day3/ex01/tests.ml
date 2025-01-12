(* THIS IS THE TEST FILE FOR THE APP MODULE *)

let print_proj (name, status, grade) = print_endline (name ^ " " ^ status ^ " " ^ (string_of_int grade))

let () =
        let project1 : App.project = ("minishell", "", 0) in
        let project2 : App.project = App.success project1 in
        let project3 : App.project = App.fail project2 in
        let project4 : App.project = App.combine project2 project3 in
        print_proj project1;
        print_proj project2;
        print_proj project3;
        print_proj project4;
