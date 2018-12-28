module Leap

// #1
//let leapYear (year: int): bool = 
//    if year % 4 = 0 && year % 100 <> 0
//    then true
//    elif year % 400 = 0
//    then true
//    else false

// #2
//let leapYear (year: int): bool =
//    match year % 4 = 0, year % 400 = 0, year % 100 = 0 with 
//        | true, true, _ -> true
//        | true, false, false -> true
//        | true, false, true -> false
//        | _ -> false

let leapYear (year: int): bool =
    year % 4 = 0 && (year % 400 = 0 || year % 100 <> 0)