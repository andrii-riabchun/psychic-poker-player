module Parsing

open Cards

let private parseSuit symbol =
    match symbol with
    | 'D' -> Diamonds
    | 'S' -> Spades
    | 'H' -> Hearts
    | 'C' -> Clubs
    |  _  -> failwithf "unknown suit '%c'" symbol

let private parseValue symbol = 
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

let private parseCard (card: string) =
    match card.ToCharArray() with
    | [|value; suit|] -> (parseValue value, parseSuit suit)
    | _ -> failwithf "invalid card code '%s'" card
    
let parse (input: string) : (Hand * Deck) =
    let cards = input.Split(' ') |> Array.map parseCard
    cards |> Array.length |> function
    | 10 -> 
        let hand = Array.sub cards 0 5 |> Array.sortBy fst
        let deck = Array.sub cards 5 5 |> List.ofArray
        (hand, deck)
    | _ -> failwith "invalid input format"

let valuePretty (v:Value) =
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
    | _ -> failwith "unexpected enum value of %s" v

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