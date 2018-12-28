module GradeSchool

type School = Map<int, string List>

let empty: School = Map.empty

let add student grade (school: School) = 
    let students =
        match school.TryFind grade with
        | Some students -> students @ [student] |> List.sort
        | _ -> [student]
    school.Add(grade, students)

let roster school =
    school |> Map.toList

let grade number (school: School) =
    match school.TryFind number with
    | Some students -> students
    | _ -> []


// Remeber! 
// let newSchool = school.Remove grade // No need to remove, Add replaces the binding.