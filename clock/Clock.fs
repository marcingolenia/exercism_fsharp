module Clock

[<Measure>] type min
[<Measure>] type h

type Clock = { Hours: int<h>; Minutes:int<min> }

let create hours minutes =
    let trueHours = (minutes / 60 + hours) % 24 * 1<h>
    let trueMinutes = minutes % 60 * 1<min>
    { Hours = trueHours; Minutes = trueMinutes }

let display clock =
    sprintf "%02i:%02i" clock.Hours clock.Minutes

let add minutes clock = failwith "You need to implement this function."

let subtract minutes clock = failwith "You need to implement this function."