open System
open System.IO

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)


let rec mapUntil (condition: array<int> -> bool) (mapFunction: array<int> -> array<int>) arr acc =
    if condition arr then
        List.rev acc
    else
        let newMapping = mapFunction arr
        mapUntil condition mapFunction newMapping (newMapping::acc)

let history = 
    File.ReadLines "./input"
    |> Seq.toArray
    // Generate the initial sequences
    |> Array.map (split [|' '|] >> Array.map int)
    // Calculate all sub sequences until all 0 (keeping intermediary sequences)
    |> Array.map (fun history -> 
        mapUntil (Array.forall ((=) 0))
            (Array.pairwise >> Array.map (fun t -> (snd t) - (fst t))) 
            history // initial
            [history]) // accumulator

let solution =
    history
    // Calculate predictions (sum of all the last elements of each sequence)
    |> Array.map (List.map Array.last >> List.sum)
    |> Array.sum

printfn "%A" solution

let solution2 = 
    history
    |> Array.map (List.map (fun a -> a.[0]) >> List.reduceBack (fun a b -> -(b-a)))
    |> Array.sum

printfn "%A" solution2