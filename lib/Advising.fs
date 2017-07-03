module Advising
open Cards
open Utils

let discard deck hand card : (Deck * Hand) =
    match deck |> List.ofSeq with
    | [] -> failwith "cannot discard any more"
    | newCard::remainingDeck -> 
        let newHand = hand |> Seq.findAndSwap card newCard
        (remainingDeck |> Seq.ofList, newHand)

let rec discardAll deck hand cards =
    match cards |> List.ofSeq with
    | [] -> hand
    | h::t -> 
        let (deck, hand) = discard deck hand h
        discardAll deck hand t

let getAllPossibleHands deck hand = 
    Seq.allCombinations hand
    |> Seq.map (fun cards -> cards |> List.ofSeq |> discardAll deck hand)

let calculateRanks =
    Seq.map (
        (fun hand -> Seq.sortBy fst hand) >> // sort by card value
        (fun hand -> (hand, Hand.score hand))   // return hand with its score
        )

let getCardsToDiscard initial (final, rank) = (Seq.except final initial, rank)

let takeBestRank = Seq.maxBy snd

let bestRank hand deck =
    hand
    |> getAllPossibleHands deck
    |> calculateRanks
    |> takeBestRank
    |> getCardsToDiscard hand