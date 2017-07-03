open System
open Cards
open Advising

let parse (input: string) : (Hand * Deck) =
    let cards = input.Split(' ') |> Array.map Card.parse
    match cards |> Seq.length with
    | 10 -> 
        let hand = Array.sub cards 0 5 |> Seq.ofArray
        let deck = Array.sub cards 5 5 |> Seq.ofArray
        (hand, deck)
    | _ -> failwith "invalid input format"

let tryParse input =
    try input |> parse |> Some
    with ex -> 
        printf "%s" ex.Message 
        None

let string cards =
    cards |> Seq.map Card.string |> String.concat " "

let lines = 
    Seq.initInfinite (fun _ -> System.Console.In.ReadLine())
    |> Seq.takeWhile(String.IsNullOrWhiteSpace >> not)

let printResult (hand, deck) = 
    let (toDiscard, rank) = bestRank hand deck
    let discardMessage = 
        match toDiscard |> List.ofSeq with
        | []    -> sprintf "You already got %A" rank
        | cards -> sprintf "Discard %s to get %A" (string cards) rank
    printfn "%s | %s | %s" (string hand) (string deck) discardMessage

[<EntryPoint>]
let main argv =
    lines |> Seq.iter (parse >> printResult) 
    0