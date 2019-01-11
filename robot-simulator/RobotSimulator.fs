module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let create direction position =
    {direction = direction; position = position}

let private turnRobot command robot =
    match (command, robot.direction) with
    | ('R', Direction.North) | ('L', Direction.South) -> { robot with direction = Direction.East }
    | ('R', Direction.East) | ('L', Direction.West) -> { robot with direction = Direction.South }
    | ('R', Direction.South) | ('L', Direction.North) -> { robot with direction = Direction.West }
    | ('R', Direction.West) | ('L', Direction.East) -> { robot with direction = Direction.North }
    | _ -> failwith "Invalid direction"

let private advance ({direction = direction; position = (x, y)} as robot) =
    match direction with
    | Direction.North -> { robot with position = (x, y + 1)}
    | Direction.East  -> { robot with position = (x + 1, y)}
    | Direction.South -> { robot with position = (x, y - 1)}
    | Direction.West  -> { robot with position = (x - 1, y)}

let private executeCommand robot command =
    match command with
    | 'R' | 'L' -> robot |> turnRobot command
    | 'A' -> robot |> advance
    | _ -> failwith "Incorrect command"

let move instructions robot =
    (robot, instructions) 
        ||> Seq.fold (fun robotState command -> executeCommand robotState command)