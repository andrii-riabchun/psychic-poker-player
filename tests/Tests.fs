module Tests

open System
open Xunit

open Cards
open Advising
open Program

[<Fact>]
let ``Should parse valid string`` () =
    let expectedHand = [| 
        (Value.Ten,     Hearts)
        (Value.Jack,    Hearts)
        (Value.Queen,   Clubs)
        (Value.Queen,   Diamonds)
        (Value.Queen,   Spades) 
        |] 
        
    let expectedDeck = [|
        (Value.Queen,   Hearts)
        (Value.King,    Hearts)
        (Value.Ace,     Hearts)
        (Value.Two,     Spades)
        (Value.Six,     Spades)
        |]
    let (hand, deck) = parse "TH JH QC QD QS QH KH AH 2S 6S"
    Assert.Equal<Hand>(expectedHand, hand)
    Assert.Equal<Deck>(expectedDeck |> List.ofArray, deck)


[<Theory>]
[<InlineData("TH1 JH QC QD QS QH KH AH 2S 6S")>]    // wrong card literal
[<InlineData("NH JH QC QD QS QH KH AH 2S 6S")>]     // invalid value
[<InlineData("TN JH QC QD QS QH KH AH 2S 6S")>]     // invalid suit
[<InlineData("TN JH QC QD QS QH KH AH 2S 6S ??")>]  // num of provided cards > 10
[<InlineData("TN JH QC QD QS QH KH AH 2S")>]        // num of provided cards < 10
let ``Should fail on parsing invalid strings``(input:string) : unit =
    (fun () -> input |> parse |> ignore)
    |> Assert.Throws<Exception> 
    |> ignore


[<Theory>]
[<InlineData("TH JH QC QD QS QH KH AH 2S 6S", Rank.RoyalFlush)>]
[<InlineData("2H 2S 3H 3S 3C 2D 3D 6C 9C TH", Rank.FourOfKind)>]
[<InlineData("2H 2S 3H 3S 3C 2D 9C 3D 6C TH", Rank.FullHouse)>]
[<InlineData("2H AD 5H AC 7H AH 6H 9H 4H 3C", Rank.Flush)>]
[<InlineData("AC 2D 9C 3S KD 5S 4D KS AS 4C", Rank.Straight)>]
[<InlineData("KS AH 2H 3C 4H KC 2C TC 2D AS", Rank.ThreeOfKind)>]
[<InlineData("AH 2C 9S AD 3C QH KS JS JD KD", Rank.TwoPair)>]
[<InlineData("6C 9C 8C 2D 7C 2H TC 4C 9S AH", Rank.Pair)>]
[<InlineData("3D 5S 2H QD TD 6S KH 9H AD QH", Rank.HighCard)>]
let ``Should calculate best discard result properly`` (input:string) (expected:Rank) : unit =
    let got = input |> parse ||> bestRank
    Assert.Equal(expected, snd got)