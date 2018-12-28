module Bob

open System.Globalization

// # 1 way
//let response (input: string): string =
//    let trimmedInput = input.Trim()
//    if trimmedInput = "" 
//        then "Fine. Be that way!"
//    elif Seq.exists (fun elem -> elem |> System.Char.IsLetter) trimmedInput && 
//        trimmedInput.ToUpper(CultureInfo.InvariantCulture) = trimmedInput
//        then "Whoa, chill out!"
//    else
//        let lastCharacter = trimmedInput.[trimmedInput.Length - 1]
//        match lastCharacter with
//            | '?' -> "Sure."
//            | _ -> "Whatever."


// 2 way
let isYelling (input: string) = 
    Seq.exists (fun elem -> elem |> System.Char.IsLetter) input && 
        input.ToUpper(CultureInfo.InvariantCulture) = input

let isAskingQuestion (input: string) =
    if input.[input.Length - 1] = '?' then true else false

let response (input: string): string =
    let trimmedInput = input.Trim()
    match trimmedInput with
        | "" -> "Fine. Be that way!"
        | _ when isYelling(trimmedInput) && isAskingQuestion(trimmedInput) -> "Calm down, I know what I'm doing!"
        | _ when isYelling(trimmedInput) -> "Whoa, chill out!"
        | _ when isAskingQuestion(trimmedInput) -> "Sure."
        | _ -> "Whatever."