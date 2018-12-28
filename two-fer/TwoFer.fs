module TwoFer

let twoFer (input: string option): string = 
    match input with
       | Some givenValue -> sprintf "One for %s, one for me." givenValue
       | None -> "One for you, one for me."