(* THIS IS THE TEST FILE FOR THE APP MODULE *)

let () =
        let project1 : App.project = ("minishell", "", 0) in
        let project2 : App.project = App.success project1 in
        let project3 : App.project = App.fail project2 in
        let project4 : App.project = App.combine project2 project3 in
        App.print_proj project1;
        App.print_proj project2;
        App.print_proj project3;
        App.print_proj project4;
