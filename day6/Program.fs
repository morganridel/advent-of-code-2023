open System.IO
open System
// For more information see https://aka.ms/fsharp-console-apps

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)

let solve (duration: float) (best: float) =
    let leftBound = 0.5*(duration - Math.Sqrt((duration*duration)-4.0*best))
    let rightBound = 0.5*(duration + Math.Sqrt((duration*duration)-4.0*best))

    [(floor (leftBound+1.0))..(ceil (rightBound-1.0))]
let solution = 
    File.ReadLines "./input"
    |> Seq.map (split ([|' '|]) >> Seq.skip 1 >> Seq.map int)
    |> Seq.transpose
    |> Seq.map (Seq.toArray >> (fun race -> solve race.[0] race.[1]) >> Seq.length)
    |> Seq.reduce (fun a b -> a*b)

printfn "Part 1: %A" solution

let solution2 = 
    File.ReadLines "./input"
    |> Seq.map ((String.filter Char.IsDigit) >> float)
    |> (Seq.toArray >> (fun race -> solve race.[0] race.[1]) >> Seq.length)

printfn "Part 2: %A" solution2
