open System
open System.IO

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)

let add (a1, a2) (b1, b2) = 
  a1 + b1, a2 + b2

// https://stackoverflow.com/questions/49888944/f-find-index-of-element-in-2d-array
let find2D needle (arr: char [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then failwith "Element not found"
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y] = needle   then (x,y)
          else go (x+1) y
    go 0 0

let maze = 
    File.ReadLines "./input"
    |> Seq.toArray
    |> Array.map Array.ofSeq
    |> array2D 

let start = find2D 'S' maze

printfn "Start point %A" start

type Direction =
    | North
    | South
    | East
    | West
let pipeToDirection c =
    match c with
    | '|' -> (North, South)
    | '-' -> (East, West)
    | 'L' -> (North, East)
    | 'J' -> (North, West)
    | '7' -> (South, West)
    | 'F' -> (South, East)
    | _   -> failwith "Invalid pipe shape"

let oneWayFromDirection (origin: Direction) (direction: Direction*Direction) =
    match direction with
    | (f, s) when f = origin -> s
    | (f, s) when s = origin -> f
    | _ -> failwith "Can't move"

let directionToMove direction =
    match direction with
    | North -> (-1, 0)
    | South -> (1, 0)
    | East -> (0, 1)
    | West -> (0, -1)

let opposite direction =
    match direction with
    | North -> South
    | South -> North
    | East -> West
    | West -> East

let traverse (maze: char [,]) (start: (int*int)) =
    // determine starting pipe directions
    let allowedDirections = 
        [North; South; East; West]
        |> List.filter (fun d ->
            let neighbor = add start (directionToMove d)
            match neighbor with
            | (a,b) when a < 0 || a > maze.GetLength 0 || b < 0 || b > maze.GetLength 1 -> false
            | _ -> match maze.[(fst neighbor), (snd neighbor)] with
                    | '.' -> false
                    | p -> match pipeToDirection p with
                                | (a, b) when a = opposite d || b = opposite d -> true
                                | _ -> false)
    
    let mutable state = maze
        
    let rec loop (maze: char [,]) (points: ((int*int)*Direction) List) step (loopPoints: (int*int) list) =
        let convertPoint (point:((int*int)*Direction)) =
            let moveDirection = 
                maze.[(fst point |> fst), (fst point |> snd)]
                |> pipeToDirection
                |> oneWayFromDirection (snd point)
            
            ((add (fst point) (directionToMove moveDirection)), opposite moveDirection)

        let updatedPoints = (List.append (List.map fst points) loopPoints)

        match points with
        | h::t when t |> List.map fst |> List.forall ((=) (fst h)) -> (step, updatedPoints) // all points arrived at same place
        | p -> loop maze (List.map convertPoint p) (step+1) updatedPoints
    
    let nextPoints =
        allowedDirections
        |> List.map (fun d -> ((add (directionToMove d) start), opposite d))

    loop maze nextPoints 1 [start]

let solution1 = traverse maze start

printfn "%A" (fst solution1)

let mutable area = 0
let mutable inside = false

for x in 0 .. maze.GetLength(0) - 1 do
    for y in 0 .. maze.GetLength(1) - 1 do
        match ((x, y),inside) with
        | t,_ when List.contains t (snd solution1) -> printf "%c" maze.[(fst t),(snd t)]
        | _,true -> printf "I"
        | _,false -> printf "O"
        match (x, y) with
        | t when List.contains t (snd solution1) -> 
            match maze.[(fst t),(snd t)] with
            | 'S' | '|' | 'L' | 'J' -> inside <- not inside
            | _ -> inside <- inside
        | _ when inside -> area <- area+1
        | _ -> area <- area
    inside <- false
    printfn ""

printfn "%d" area