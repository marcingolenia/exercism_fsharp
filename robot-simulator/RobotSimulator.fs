module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let create direction position =
    {direction = direction; position = position}

let turnRobot command robot =
    match (command, robot.direction) with
    | ('R', Direction.North) | ('L', Direction.South) -> { robot with direction = Direction.East }
    | ('R', Direction.East) | ('L', Direction.West) -> { robot with direction = Direction.South }
    | ('R', Direction.South) | ('L', Direction.North) -> { robot with direction = Direction.West }
    | ('R', Direction.West) | ('L', Direction.East) -> { robot with direction = Direction.North }
    | _ -> failwith "Invalid direction"

let advance robot =
    match robot.direction with
    | Direction.North -> { robot with position = (robot.position |> fst, (robot.position |> snd) + 1)}
    | Direction.East -> { robot with position = ((robot.position |> fst) + 1, robot.position |> snd)}
    | Direction.South -> { robot with position = (robot.position |> fst, (robot.position |> snd) - 1)}
    | Direction.West -> { robot with position = ((robot.position |> fst) - 1, robot.position |> snd)}
    | _ -> failwith "Invalid direction"

let executeCommand robot command =
    match command with
    | 'R' | 'L' -> robot |> turnRobot command
    | 'A' -> robot |> advance
    | _ -> failwith "Incorrect command"

let move instructions robot =
    instructions 
                |> List.ofSeq 
                |> List.fold (fun robotState command -> executeCommand robotState command) robot