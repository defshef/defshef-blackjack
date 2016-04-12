module Blackjack

type Picture =
    | Ace
    | Jack
    | Queen
    | King

type Face =
    | Number of int
    | Face of Picture

type Suit =
    | Spade
    | Club
    | Heart
    | Diamond

type Card = | Card of Suit * Face

type Deck = Deck of Card list

type Shoe = Shoe of Card list

type Hand = Hand of Card list

type HandValue =
    | Bust of int
    | BlackJack
    | Soft of int
    | Hard of int

type Game = {
    Deck : Deck
    Dealer : Hand
    Players : Hand list
}

let newDeck : Deck =
    let cards (suit : Suit) =
        seq {
            yield Card(suit, Face(Ace))
            for i in 2..10 do
                yield Card(suit, Number(i))
            yield Card(suit, Face(Jack))
            yield Card(suit, Face(Queen))
            yield Card(suit, Face(King))
        }

    let allCards =
        seq {
            for s in [Club; Heart; Diamond; Spade] do
                yield! (cards s)
        }
        |> Seq.toList
    Deck(allCards)

let shuffle (deck : Deck) : Deck =
    deck

let take (deck : Deck) : (Card * Deck) =
    failwith "NI"

let deal handCount (deck : Deck) : (Hand list * Deck) =
    failwith "NI"

let getHandValue (Hand(hand)) : HandValue =
    failwith "NI"

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code