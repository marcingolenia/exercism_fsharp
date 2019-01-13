module OcrNumbers

let [<Literal>] Ocr0 = " _ | ||_|"
let [<Literal>] Ocr1 = "     |  |"
let [<Literal>] Ocr2 = " _  _||_ "
let [<Literal>] Ocr3 = " _  _| _|"
let [<Literal>] Ocr4 = "   |_|  |"
let [<Literal>] Ocr5 = " _ |_  _|"
let [<Literal>] Ocr6 = " _ |_ |_|"
let [<Literal>] Ocr7 = " _   |  |"
let [<Literal>] Ocr8 = " _ |_||_|" 
let [<Literal>] Ocr9 = " _ |_| _|"

let toStringNumber inputString =
  match inputString with
    | Ocr0 -> "0"
    | Ocr1 -> "1"
    | Ocr2 -> "2"
    | Ocr3 -> "3"
    | Ocr4 -> "4"
    | Ocr5 -> "5"
    | Ocr6 -> "6"
    | Ocr7 -> "7"
    | Ocr8 -> "8"
    | Ocr9 -> "9"
    | _ -> "?"

let (|IsMultipleOf4|_|) number =
  if number % 4 = 0 then Some IsMultipleOf4 else None

let (|IsMultipleOf3|_|) number =
  if number % 3 = 0 then Some IsMultipleOf3 else None

let validate (input: string list) =
  match (input.Length, input.[0].Length) with
    | (IsMultipleOf4, IsMultipleOf3) -> Ok input
    | _ -> Error "Incorrect input"

let convertHorizontaly (input: string list) =
  let columnsToConvert = (input.[0].Length / 3 - 1)
  ("", [0..columnsToConvert])
    ||> List.fold (fun state iterationNo ->
      let takeFrom = iterationNo * 3
      let takeTo = iterationNo * 3 + 2
      state + (System.String.Concat(List.concat[
        (input.[0] |> Seq.toList).[(takeFrom)..(takeTo)];
        (input.[1] |> Seq.toList).[(takeFrom)..(takeTo)];
        (input.[2] |> Seq.toList).[(takeFrom)..(takeTo)];
        ]) |> toStringNumber))

let convertVerticallyAndHorizontally (input: string list) =
  let rowsToConvert = input.Length / 4 - 1
  ("", [0..rowsToConvert])
    ||> List.fold (fun state iterationNo ->
      let takeFrom = iterationNo * 4
      let takeTo = iterationNo * 4 + 3
      (if(iterationNo > 0) then state + "," else "") + convertHorizontaly input.[takeFrom..takeTo] )

let convert (input: string list) =
  match (input |> validate) with
    | Ok _ -> Some(convertVerticallyAndHorizontally input)
    | Error _ -> None