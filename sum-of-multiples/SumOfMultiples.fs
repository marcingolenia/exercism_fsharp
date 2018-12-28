module SumOfMultiples

let multiplesForNumber number upperBound =
    [ number .. number .. upperBound - 1 ]

let sum numbers upperBound =
    numbers 
        |> Seq.map (fun number -> multiplesForNumber number upperBound)
        |> Seq.concat
        |> Seq.distinct
        |> Seq.sum