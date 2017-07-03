open System
open Cards

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
    with ex -> None

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code