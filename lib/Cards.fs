module Cards

type Value = 
    | Two   = 2
    | Three = 3
    | Four  = 4
    | Five  = 5
    | Six   = 6
    | Seven = 7
    | Eight = 8
    | Nine  = 9
    | Ten   = 10
    | Jack  = 11
    | Queen = 12
    | King  = 13
    | Ace   = 14

module Value =
    let parse =
        function
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
        |  v  -> failwithf "unknown value literal '%c'" v

    let tryParse value =
        try value |> parse |> Some
        with ex -> None

    let string =
        function
        | Value.Two     -> "2"
        | Value.Three   -> "3"
        | Value.Four    -> "4"
        | Value.Five    -> "5"
        | Value.Six     -> "6"
        | Value.Seven   -> "7"
        | Value.Eight   -> "8"
        | Value.Nine    -> "9"
        | Value.Ten     -> "T"
        | Value.Jack    -> "J"
        | Value.Queen   -> "Q"
        | Value.King    -> "K"
        | Value.Ace     -> "A"
        | _             -> failwith "illegal value"

type Suit =
    | Diamonds
    | Spades
    | Hearts
    | Clubs

module Suit = 
    let parse =
        function
        | 'D' -> Diamonds
        | 'S' -> Spades
        | 'H' -> Hearts
        | 'C' -> Clubs
        |  s  -> failwithf "unknown suit literal '%c'" s

    let tryParse suit =
        try suit |> parse |> Some
        with ex -> None

    let string = 
        function
        | Diamonds  -> "D"
        | Hearts    -> "H"
        | Clubs     -> "C"
        | Spades    -> "S" 

type Card = Value * Suit

module Card =
    let parse (card: string) =
        match card.ToCharArray() with
        | [|value; suit|] -> (Value.parse value, Suit.parse suit)
        | _ -> failwithf "invalid card code '%s'" card

    let tryParse card =
        try card |> parse |> Some
        with ex -> None

    let string card = 
        let (value, suit) = card
        [Value.string value; Suit.string suit] |> String.concat ""

type Hand = Card seq

type Deck = Card seq

type Rank =
    | HighCard      = 1
    | Pair          = 2
    | TwoPair       = 3
    | ThreeOfKind   = 4
    | Straight      = 5
    | Flush         = 6
    | FullHouse     = 7
    | FourOfKind    = 8
    | StraightFlush = 9
    | RoyalFlush    = 10
 
module Hand = 
    let isFlush hand = 
        hand
        |> Seq.pairwise
        |> Seq.map (fun ((_, suit1), (_,suit2)) -> (suit1, suit2))
        |> Seq.forall (fun (suit1, suit2) -> suit1 = suit2)


    let isStraight hand =
        match hand |> Seq.map (fst>>int) |> List.ofSeq with
        | [2;3;4;5;14] -> true // baby straigh
        | _-> hand 
              |> Seq.pairwise
              |> Seq.map (fun ((val1, _), (val2, _)) -> (val1, val2))
              |> Seq.forall (fun (val1, val2) -> int val2 = int val1 + 1)


    let getValueCounts hand = 
        hand
        |> Seq.countBy (fun (value, _) -> value)
        |> Seq.map (fun (_, count) -> count)
        |> Seq.toList
        |> List.sort
        |> List.rev

    let highestCardIs value hand = hand |> Seq.last |> fst = value

    let score hand =
        match (isStraight hand, isFlush hand) with
        | (true, true) -> 
            if hand |> highestCardIs Value.Ace 
            then Rank.RoyalFlush
            else Rank.StraightFlush
        | (false, true) -> Rank.Flush
        | (true, false) -> Rank.Straight
        | (false, false) -> 
            match getValueCounts hand with
            | [4;1]     -> Rank.FourOfKind
            | [3;2]     -> Rank.FullHouse
            | [3;1;1]   -> Rank.ThreeOfKind
            | [2;2;1]   -> Rank.TwoPair
            | [2;1;1;1] -> Rank.Pair
            | _         -> Rank.HighCard