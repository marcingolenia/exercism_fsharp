module Raindrops

let factorsToWordLookup = 
    Map.empty.
        Add(3, "Pling").
        Add(5, "Plang").
        Add(7, "Plong")  
    
let convert number =
    let raindropTranslation =
        factorsToWordLookup |> 
        Map.fold (fun state key value ->
            if number % key = 0 then state + value else state) ""
    if raindropTranslation.Length = 0 then number.ToString() else raindropTranslation
    
