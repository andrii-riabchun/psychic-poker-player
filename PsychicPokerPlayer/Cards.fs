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
   
type Suit =
| Diamonds
| Spades
| Hearts
| Clubs

type Card = Value * Suit

type Hand = Card array

type Deck = Card list

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

let isFlush (hand:Hand) = 
    hand
    |> Seq.pairwise
    |> Seq.map (fun ((_, suit1), (_,suit2)) -> (suit1, suit2))
    |> Seq.forall (fun (suit1, suit2) -> suit1 = suit2)


let isStraight (cards : Hand) =
    match cards |> Array.map (fst>>int) |> List.ofArray with
    | [2;3;4;5;14] -> true // baby straigh
    | _-> cards 
          |> Seq.pairwise
          |> Seq.map (fun ((val1, _), (val2, _)) -> (val1, val2))
          |> Seq.forall (fun (val1, val2) -> int val2 = int val1 + 1)


let getValueCounts (hand : Hand) = 
    hand
    |> Seq.countBy (fun (value, _) -> value)
    |> Seq.map (fun (_, count) -> count)
    |> Seq.toList
    |> List.sort
    |> List.rev

let scoreHand (hand:Hand) =
    match (isStraight hand, isFlush hand) with
    | (true, true) -> 
        if hand |> Array.last |> fst = Value.Ace 
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