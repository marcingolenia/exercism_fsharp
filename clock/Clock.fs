module Clock

[<Measure>] type min
[<Measure>] type h

[<Literal>] 
let minutesInDay = 1440<min>

[<Literal>] 
let minutesPerHour = 60<min/h>  

let toMinutes hours =
    hours * minutesPerHour

let toHours minutes =
    minutes / minutesPerHour

type Clock = { Hours: int<h>; Minutes:int<min> }

let getTotalMinutes clock =
    (clock.Hours |> toMinutes) + clock.Minutes 

let add minutes clock = 
    let minutesToAdd = minutes * 1<min> % minutesInDay
    let timeToSetInMinutes = (minutesInDay + getTotalMinutes clock + minutesToAdd) % minutesInDay
    {Hours = timeToSetInMinutes |> toHours; Minutes = timeToSetInMinutes % 60<min>}

let create hours minutes =
    let totalMinutes = minutes * 1<min> + (hours * 1<h> |> toMinutes)
    add (totalMinutes/1<min>) {Hours = 0<h>; Minutes = 0<min>}

let display clock =
    sprintf "%02i:%02i" clock.Hours clock.Minutes

let subtract minutesToSubstruct clock =
    let minutesToSubstruct = minutesToSubstruct * 1<min> % minutesInDay
    let timeToSetInMinutes = (minutesInDay + getTotalMinutes clock - minutesToSubstruct) % minutesInDay
    {Hours = timeToSetInMinutes |> toHours; Minutes = timeToSetInMinutes % 60<min>}