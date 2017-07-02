module Advising
open Cards

let private swap oldElem newElem arr =
    arr 
    |> Array.map (fun v -> if v=oldElem then newElem else v) 

type private Combination<'a> = 'a list

let rec comb n l =
    match (n,l) with
    | (0,_) -> [[]]
    | (_,[]) -> []
    | (n,x::xs) ->
        let useX = List.map (fun l -> x::l) (comb (n-1) xs)
        let noX = comb n xs
        useX @ noX

let possibleDiscardResults (hand:Hand) = [ for i in 0..5 do yield! comb i (List.ofArray hand) ]

let discard deck hand (card:Card) : (Deck * Hand) =
    match deck with
    | [] -> failwith "cannot discard any more"
    | newCard::remainingDeck -> 
        let newHand = hand |> swap card newCard
        (remainingDeck, newHand)

let rec discardAll (deck:Deck) (hand: Hand) (cards: Card list) : Hand =
    match cards with
    | [] -> hand
    | h::t -> 
        let (newDeck, newHand) = discard deck hand h
        discardAll newDeck newHand t

let bestRank (hand:Hand) deck  =
    hand
    |> possibleDiscardResults
    |> Seq.map (fun cards -> discardAll deck hand cards)
    |> Seq.map (fun hand -> Array.sortBy fst hand)
    |> Seq.map (fun hand -> (hand, scoreHand hand))
    |> Seq.maxBy snd