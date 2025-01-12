type project = string * string * int

let zero = ("", "", 0)

let combine (p1, s1, g1) (p2, s2, g2) =
        let project_name = p1 ^ p2 in
        let grade = (g1 + g2) / 2 in
        let status = if grade > 80 then "succeed" else "failed" in
        (project_name, status, grade)

let fail (p1, _, _) = (p1, "failed", 0)

let success (p1, _, _) = (p1, "succeed", 80) (* The assignment is not very clear here. In the first place, for a success, average needs to be ABOVE 80 but here it has to be set to 80. *)

let print_proj (name, status, grade) = print_endline (name ^ " " ^ status ^ " " ^ (string_of_int grade))
