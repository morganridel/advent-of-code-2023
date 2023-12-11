open System
open System.IO

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)

let rec printSeq (s: seq<'T>)=
    match s with
    | :? seq<seq<'T>> as nestedSeq ->
        for innerSeq in nestedSeq do
            printSeq innerSeq
    | _ ->
        for item in s do
            printf "%A " item
        printfn ""

let rec printSeqWithNewLines (s: seq<'T>)=
    match s with
    | :? seq<seq<'T>> as nestedSeq ->
        for innerSeq in nestedSeq do
            printSeq innerSeq
    | _ ->
        for item in s do
            printf "%A " item
            printfn ""
        printfn ""

let findCoordinates<'T> (predicate: 'T -> bool) (matrix: list<list<'T>>) =
    matrix
    |> List.indexed
    |> List.collect (fun (rowIndex, row) ->
        row
        |> List.indexed
        |> List.filter (fun (_, element) -> predicate element)
        |> List.map (fun (colIndex, _) -> (rowIndex, colIndex))
    )

let initialSpace = 
    File.ReadLines "./input"
    |> Seq.toList

let emptySpace (s:string) = 
    s.ToCharArray() |> Array.forall ((=) '.')

let expandVertically =
    fun (s: string) -> 
    match s with
    | s when emptySpace s -> [s;s] // Expand!
    | _ -> [s]

let transposeStringList (list: string list) =
    list
    |> List.map ((fun s -> s.ToCharArray()) >> Array.toList)
    |> List.transpose
    |> List.map (List.toArray >> String)
let expandedSpace = 
    initialSpace
    |> List.collect expandVertically
    |> transposeStringList
    |> List.collect expandVertically // actually expand horizontally
    |> transposeStringList

let expandedIndicesRow =
    initialSpace
    |> List.indexed
    |> List.filter (fun (_,s) -> emptySpace s)
    |> List.map fst

let expandedIndicesColumn =
    initialSpace
    |> transposeStringList
    |> List.indexed
    |> List.filter (fun (_,s) -> emptySpace s)
    |> List.map fst

// printfn "%A %A" expandedIndicesRow expandedIndicesColumn

let galaxies =
    expandedSpace
    |> List.map Seq.toList
    |> findCoordinates ((=) '#')

// printSeq galaxies

let distance (a:int32*int32) (b:int32*int32) =
    (abs (fst b - fst a)) + (abs (snd b - snd a))

let distanceWithExpansion (multiple: int64) (a:int32*int32) (b:int32*int32) =
    let xCrossedExpansion = 
        expandedIndicesRow 
        |> List.filter (fun i -> i > min (fst a) (fst b) && i < max (fst a) (fst b))
        |> List.length
        |> int64
    
    let yCrossedExpansion = 
        expandedIndicesColumn 
        |> List.filter (fun i -> i > min (snd a) (snd b) && i < max (snd a) (snd b))
        |> List.length
        |> int64

    // printfn "between %A and %A, expanded space is crossed (%d,%d) times" a b xCrossedExpansion yCrossedExpansion

    let originalDistance = (abs (fst b - fst a)) + (abs (snd b - snd a)) |> int64

    originalDistance + (xCrossedExpansion*multiple) + (yCrossedExpansion*multiple) - xCrossedExpansion - yCrossedExpansion

// https://stackoverflow.com/questions/3851296/f-return-element-pairs-in-list
let rec pairs l = [
    match l with 
    | h::t -> for e in t do yield h, e
              yield! pairs t
    | _ -> () ]

let solution1 =
    pairs galaxies
    |> List.map (fun (a, b) -> distance a b)
    |> List.sum

printfn "%d" solution1

// Part 2

let solution2 =
    initialSpace
    |> List.map Seq.toList
    |> findCoordinates ((=) '#')
    |> pairs
    |> List.map (fun (a, b) -> distanceWithExpansion 1_000_000 a b)
    |> List.sum

printfn "%d" solution2