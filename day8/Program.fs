open System
open System.IO

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)


let input = File.ReadLines "./input"
let direction = Seq.head input
let parseTupleString (str:string) =
    let comma = str.IndexOf(',')
    let firstString = str.[1..comma-1].Trim()
    let secondString = str.[comma+2..str.Length-2].Trim()
    (firstString, secondString)
let graph =
    input
    |> Seq.tail
    |> Seq.skip 1
    |> Seq.map (split [|'='|] >> (fun a -> (a.[0], a.[1] |> parseTupleString)))
    |> dict

let rec traverseGraph (graph:Collections.Generic.IDictionary<string,(string * string)>) (direction: string) (start: string) (step: int) =
    // printfn "currently on %s, going to the %c to %A" start (direction.[step % direction.Length]) (graph.TryFind start)
    let nextDirection = 
        match direction.[step % direction.Length] with
        | 'L' -> fst
        | 'R' -> snd
        | _ -> failwith "Unexpected direction"

    match (start, graph.Item start) with
    | "ZZZ", _ -> step // finished
    | _, _ when step > 1_000_000 -> step // Safety limit
    | _, node -> traverseGraph graph direction (nextDirection node) (step+1)

let solution1 = traverseGraph graph direction "AAA" 0

printfn "Solution part 1: %d" solution1
let (|StrEndsInZ|_|) (str:string) =
    //if (l |> Seq.filter (fun (str:string) -> str.[str.Length - 1] = 'Z') |> Seq.length |> ((=) (Seq.length l))) then Some EndsInZ else None
    if (str.[str.Length - 1] = 'Z') then Some StrEndsInZ else None

let rec traverseGraphUntilZ (graph:Collections.Generic.IDictionary<string,(string * string)>) (direction: string) (start: string) (step: int64) =
    // printfn "currently on %s, going to the %c to %A" start (direction.[step % direction.Length]) (graph.TryFind start)
    let directionIndex = step % (int64(direction.Length))
    let nextDirection = 
        match direction.[int directionIndex] with
        | 'L' -> fst
        | 'R' -> snd
        | _ -> failwith "Unexpected direction"

    match (start, graph.Item start) with
    | StrEndsInZ, _ ->  step
    | _, _ when step > 1_000_000 -> step // Safety limit
    | _, node -> traverseGraphUntilZ graph direction (nextDirection node) (step+1L)
    | _, _ -> failwith "Node not found in graph"

let endsWithA = graph.Keys |> Seq.where (fun str -> str.[str.Length - 1] = 'A') |> Seq.toArray

// After trying for too long with never ending recursion, a smarter way is needed
// Somehow, the path is cyclical and encounters a Z ending string at the same interval
let stepsBeforeZ = 
    endsWithA
    |> Array.map (fun start -> traverseGraphUntilZ graph direction start 0)

// So we have to find the least common multiple between the different interval, to find where they all land on a Z

// from https://gist.github.com/krishnabhargav/da6686e295638d000aab
let rec gcd a b = 
    match (a,b) with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y) -> gcd x (y-x)
let lcm a b = a*b/(gcd a b)
// --------------------

let solution2 = 
    stepsBeforeZ
    |> Array.reduce lcm

printfn "Solution part 2: %d" solution2