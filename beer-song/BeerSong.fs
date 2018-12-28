module BeerSong

let recite (startBottles: int) (takeDown: int) = 
    let rec sing bottlesNo f = 
        match bottlesNo with
        | 0 -> f() @ ["No more bottles of beer on the wall, no more bottles of beer."; 
         "Go to the store and buy some more, 99 bottles of beer on the wall."]
        | 1 -> sing (bottlesNo-1) (fun () -> f() @ ["1 bottle of beer on the wall, 1 bottle of beer.";
         sprintf "Take it down and pass it around, no more bottles of beer on the wall.";
         ""])
        | 2 -> sing (bottlesNo-1) (fun () -> f() @ ["2 bottles of beer on the wall, 2 bottles of beer.";
             "Take one down and pass it around, 1 bottle of beer on the wall.";
             ""])
        | _ -> sing (bottlesNo-1) (fun () -> f() @ [sprintf "%d bottles of beer on the wall, %d bottles of beer." bottlesNo bottlesNo;
             sprintf "Take one down and pass it around, %d bottles of beer on the wall." (bottlesNo - 1);
             ""])
    sing startBottles (fun () -> []) |> List.take (takeDown*3-1)