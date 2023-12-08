open System
open System.IO

let splitOptions: StringSplitOptions = (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries);
let split (separators: char array) (x:string) = x.Split(separators, splitOptions)

type CardValue =
    | A
    | K
    | Q
    | T
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | J

let fromChar (c:char) =
    match c with
        | 'A' -> A
        | 'K' -> K
        | 'Q' -> Q
        | 'J' -> J
        | 'T' -> T
        | '9' -> Nine
        | '8' -> Eight
        | '7' -> Seven
        | '6' -> Six
        | '5' -> Five
        | '4' -> Four
        | '3' -> Three
        | '2' -> Two
        | _ -> failwith "Error: Unexpected case fromChar"

type FiveCards = CardValue list

let toFiveCards (cards: CardValue list): FiveCards =
    if cards |> List.length = 5 then (cards) else failwith "Error: Unexpected case toFiveCards"

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPairs
    | OnePair
    | HighCard

type HandParser = FiveCards -> HandType

let occurences (cards: FiveCards) =
    let o = 
        cards 
        |> List.countBy id
        |> List.sortByDescending snd

    // Change the occurences count to handle Joker
    let nJoker = cards |> List.where ((=)J) |> List.length
    let oWithoutJoker = o |> List.where (fst >> (<>)J)

    // Add number of joker to the most occuring card
    printfn "%A" o
    match oWithoutJoker with
        | _ when o.Length = 1 -> o
        | head::tail -> ((fst head), (snd head)+nJoker)::tail
        | _ -> o


let (|NOfAKind|_|) (count: int) (cards: FiveCards)  =
    cards 
    |> occurences
    |> List.where (fun (_, c) -> c = count) 
    |> List.tryHead
    |> Option.map fst

let (|FullHouse|_|) (cards: FiveCards)  =
    match occurences cards with
    // Holy shit you can do this
    | [(bigValue, 3); (lowValue, 2)] -> FullHouse Some (bigValue, lowValue)
    | _ -> FullHouse None

let (|TwoPairs|_|) (cards: FiveCards)  =
    match occurences cards with
    | [(pair1, 2); (pair2, 2); (_, 1)] -> TwoPairs Some (pair1, pair2)
    | _ -> TwoPairs None

let (|OnePair|_|) (cards: FiveCards)  =
    match occurences cards with
    | (pair, 2)::tail when (List.length tail = 3) -> OnePair Some (pair)
    | _ -> OnePair None

let (|HighCard|_|) (cards: FiveCards)  =
    match occurences cards with
    | l when (List.length l = 5) -> Some HighCard
    | _ -> None

let handParser: HandParser =
    fun cards ->
        match cards with
        | NOfAKind 5 _ -> FiveOfAKind
        | NOfAKind 4 _ -> FourOfAKind
        | FullHouse (a,b) -> FullHouse
        | NOfAKind 3 _ -> ThreeOfAKind
        | TwoPairs (p1, p2) -> TwoPairs
        | OnePair (_) -> OnePair
        | HighCard -> HighCard
        | _ -> failwith "Error: Unexpected case HandParser"

let parseHand (h: string) = h.ToCharArray() |> Array.map fromChar |> Array.toList |> toFiveCards
let parseLine (str: string) =    
    str
    |> split [|' '|]
    |> function 
        | [|hand; bid|] -> (hand |> parseHand, int bid)
        | _ -> failwith "Error: Unexpected case LineParser"

let solution = 
    File.ReadLines "./input"
    |> Seq.map parseLine
    |> Seq.map (fun (hand, bid) -> ((hand, hand |> handParser), bid)) // ((FiveCards * HandType) * bid)
    |> Seq.sortByDescending (fun ((fc, t), _) -> (t, fc)) // Sort by Type first, then by cards value
    |> Seq.mapi (fun rank (_, bid) -> int64(bid * (rank+1)))
    |> Seq.sum



printfn "Part 2 %d" solution