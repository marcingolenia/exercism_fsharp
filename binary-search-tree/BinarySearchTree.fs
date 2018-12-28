module BinarySearchTree

type Tree<'T> =
    | Leaf
    | Node of value: 'T * left: Tree<'T> * right: Tree<'T>

let rec insert newValue (targetTree: Tree<'T>) =
    match targetTree with
    | Leaf -> Node(newValue, Leaf, Leaf)
    | Node (value, left, right) ->
        if newValue <= value then Node(value, insert newValue left, right)
        else Node(value, left, insert newValue right)
    | _ -> targetTree

let data node = 
    match node with
    | Node (value, _, _) -> value
    | Leaf -> failwith "Invalid Tree type."

let left node = 
    match node with 
    | Node (_, left, _) ->
        match left with
        | Node (_, _, _) -> Some left
        | Leaf -> None
    | Leaf -> failwith "Invalid Tree type."

let right node =
    match node with 
    | Node (_, _, right) ->
        match right with
        | Node (_, _, _) -> Some right
        | Leaf -> None
    | Leaf -> failwith "Invalid Tree type."

let create items = 
    List.fold (fun state item -> insert item state) Leaf items

let sortedData tree  =
    let rec sort node list =
        match node with
        | Leaf -> list
        | Node (value, left, right) ->
            sort left (value :: (sort right list))
    sort tree []
    //let rec loop tree = seq {
    //    match tree with
    //    | Leaf -> ()
    //    | Node (value, left, right) ->
    //        yield value
    //        yield! loop left
    //        yield! loop right
    //}
    //loop tree |> Seq.sort |> Seq.toList

    //let flip f = fun a b -> f b a