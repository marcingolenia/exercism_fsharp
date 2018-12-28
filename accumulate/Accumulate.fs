module Accumulate

let accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec customMap state list =
        match list with
        | [] -> state
        | head :: tail -> customMap (func head::state) tail
    customMap [] input |> List.rev
        