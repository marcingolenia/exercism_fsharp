module QueenAttackTypes
type Number0_8 = Number0_8 of int

module Number0_8 = 
    let create qty =
        if qty < 0 then
            failwith "Cannot be negative"
        else if qty > 7 then
            failwith "Cannot be greater than 8"
        else
            qty
    let value (Number0_8 qty) = qty

module QueenAttack =
    let (|CanAttackInRow|CanAttackInColumn|CanAttackInDiagonal|CantAttack|) (queenPosition1, queenPosition2) = 
        match (queenPosition1, queenPosition2) with
        | (position1, position2) when fst position1 = fst position2 -> CanAttackInColumn
        | (position1, position2) when snd position1 = snd position2 -> CanAttackInRow
        | (position1, position2) when ((fst position1) - (fst position2) |> abs) = 
            ((snd position1) - (snd position2) |> abs) -> CanAttackInDiagonal
        | _ -> CantAttack

    let create position = 
         try 
            let _ = (fst position |> Number0_8.create, snd position |> Number0_8.create)
            true
         with _ -> false

    
    let canAttack queen1 queen2 =
        let queenPosition1 = (fst queen1 |> Number0_8.create, snd queen1 |> Number0_8.create)
        let queenPosition2 = (fst queen2 |> Number0_8.create, snd queen2 |> Number0_8.create)
        match queenPosition1, queenPosition2 with
        | CantAttack -> false 
        | _ -> true
        
//type Number0to8 = private Number0to8 of int
//module Number0to8 = 
//    let create qty =
//        if qty < 0 then
//            Error "Cannot be negative"
//        else if qty > 8 then
//            Error "Cannot be greater than 8"
//        else
//            Ok (Number0to8 qty)
//    let value (Number0to8 qty) = qty
// 