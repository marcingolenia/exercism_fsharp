module KindergartenGarden

type Plant = Grass | Clover | Radishes | Violets

let students = Map.empty.
                Add("Alice", 0).
                Add("Bob", 1).
                Add("Charlie", 2).
                Add("David", 3).
                Add("Eve", 4).
                Add("Fred", 5).
                Add("Ginny", 6).
                Add("Harriet", 7).
                Add("Ileana", 8).
                Add("Joseph", 9).
                Add("Kincaid", 10).
                Add("Larry", 11)

let charToPlant letter =
    match letter with
    | 'G' -> Grass
    | 'C' -> Clover
    | 'R' -> Radishes
    | 'V' -> Violets
    | _ -> failwith (sprintf "Plant with %c not exists." letter)

let selectPlantChars startIndex characters =
    characters |> List.ofSeq |> List.skip startIndex |> List.take 2

let plants (diagram: string) student =
    let diagramRows = diagram.Split("\n")
    let studentIndex = students.[student] * 2
    let plantsCharacters = 
        (diagramRows.[0] |> selectPlantChars studentIndex) @ 
        (diagramRows.[1] |> selectPlantChars studentIndex)
    plantsCharacters |> List.fold (fun state item -> state @ [charToPlant item] ) []