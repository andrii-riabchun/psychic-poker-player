module Convertion
open Cards

module Parse = 
    let suit symbol =
        match symbol with
        | 'D' -> Diamonds
        | 'S' -> Spades
        | 'H' -> Hearts
        | 'C' -> Clubs
        |  _  -> failwithf "unknown suit '%c'" symbol

    let value symbol = 
        match symbol with
        | '2' -> Value.Two
        | '3' -> Value.Three
        | '4' -> Value.Four
        | '5' -> Value.Five
        | '6' -> Value.Six
        | '7' -> Value.Seven
        | '8' -> Value.Eight
        | '9' -> Value.Nine
        | 'T' -> Value.Ten
        | 'J' -> Value.Jack
        | 'Q' -> Value.Queen
        | 'K' -> Value.King
        | 'A' -> Value.Ace
        |  _  -> failwithf "unknown value '%c'" symbol

    let cardOf (card: string) =
        match card.ToCharArray() with
        | [|valueChar; suitChar|] -> (value valueChar, suit suitChar)
        | _ -> failwithf "invalid card code '%s'" card
        
    let input (input: string) : (Hand * Deck) =
        let cards = input.Split(' ') |> Array.map cardOf
        cards |> Array.length |> function
        | 10 -> 
            let hand = Array.sub cards 0 5 |> Seq.sortBy fst
            let deck = Array.sub cards 5 5 |> Seq.ofArray
            (hand, deck)
        | _ -> failwith "invalid input format"

let valuePretty v =
    match v with
    | Value.Two     -> '2'
    | Value.Three   -> '3'
    | Value.Four    -> '4'
    | Value.Five    -> '5'
    | Value.Six     -> '6'
    | Value.Seven   -> '7'
    | Value.Eight   -> '8'
    | Value.Nine    -> '9'
    | Value.Ten     -> 'T'
    | Value.Jack    -> 'J'
    | Value.Queen   -> 'Q'
    | Value.King    -> 'K'
    | Value.Ace     -> 'A'

let suitPretty suit = 
    match suit with
    | Diamonds  -> 'D'
    | Hearts    -> 'H'
    | Clubs     -> 'C'
    | Spades    -> 'S'  

let cardPretty card = 
    let (value, suit) = card
    [|valuePretty value; suitPretty suit|] |> System.String

let cardsPretty cards = cards |> Seq.map cardPretty |> (String.concat " ")