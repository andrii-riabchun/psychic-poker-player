# The Psychic Poker Player 

## Problem

In 5-card draw poker, a player is dealt a hand of five cards (which may be looked at). The player may then discard between zero and five of his or her cards and have them replaced by the same number of cards from the top of the deck (which is face down). The object is to maximize the value of the final hand. The different values of hands in poker are given at the end of this problem.

Normally the player cannot see the cards in the deck and so must use probability to decide which cards to discard. In this problem, we imagine that the poker player is psychic and knows which cards are on top of the deck. Write a program which advises the player which cards to discard so as to maximize the value of the resulting hand.

## Run

You need [dotnet-cli](https://www.microsoft.com/net/core) to run solution.

### Run on samples
```
> dotnet restore
> cd src
> cat sample.txt | dotnet run 
```

### Run tests
```
> dotnet test 
```
