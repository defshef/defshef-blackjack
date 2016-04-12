module Helpers =
    let rand = new System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle a =
        let arr = a |> List.toArray
        Array.iteri (fun i _ -> swap arr i (rand.Next(i, Array.length arr))) arr
        arr |> Array.toList

module Blackjack =
    type Picture =
        | Jack
        | Queen
        | King

    type Face =
        | Ace
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
        | Value of soft : int * hard : int

    type Game = {
        Deck : Deck
        Dealer : Hand
        Players : Hand list
    }

    let newDeck : Deck =
        let cards (suit : Suit) =
            seq {
                yield Card(suit, Ace)
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

    let shuffle (Deck(cards)) : Deck =
        let cards = Helpers.shuffle cards
        Deck(cards)

    let takeTop (Deck(cards)) : (Card * Deck) =
        match cards with
        | [] ->
            failwith "No cards!"
        | top :: deck ->
            (top, Deck(deck))

    let dealInitialHands numberOfHands (deck : Deck) : (Hand list * Deck) =
        let (Deck(cards)) = deck
        if (numberOfHands <= 1) then
            failwith "Too few players!"
        else if (numberOfHands * 2) > cards.Length then
            failwith "Too many players!"
        else
            let getTopN n d =
                let rec getTopNRec i d acc =
                    if i = 0 then
                        (acc, d)
                    else
                        let (c, d') = takeTop d
                        getTopNRec (i - 1) d' (c :: acc)
                getTopNRec n d []

            let (firstCards, d') = getTopN numberOfHands deck
            let (secondCards, d') = getTopN numberOfHands d'
            let hands =
                firstCards
                |> List.zip secondCards
                |> List.map (fun (firstCard, secondCard) -> [firstCard; secondCard] |> Hand )
            (hands, d')

    let getHandValue (Hand(cards)) : HandValue =
        let getHardValue (face : Face) : int =
            match face with
            | Ace -> 11
            | Number(i) -> i
            | Face (_) -> 10

        let getSoftValue (face : Face) : int =
            match face with
            | Ace -> 1
            | Number(i) -> i
            | Face (_) -> 10

        let hardValue =
            cards
            |> List.fold (fun i (Card(_, face)) -> i + (getHardValue face)) 0

        let softValue =
            cards
            |> List.fold (fun i (Card(_, face)) -> i + (getSoftValue face)) 0

        let isBlackJack =
            let faces = cards |> List.map (fun (Card(_, f)) -> f)
            match faces with
            | [Ace; f] when (getHardValue f) = 10 -> true
            | [f; Ace] when (getHardValue f) = 10 -> true
            | _ -> false

        if isBlackJack then
            BlackJack
        else if softValue > 21 then
            Bust(softValue)
        else
            Value(softValue, hardValue)

    [<EntryPoint>]
    let main argv =
        printfn "%A" argv
        0 // return an integer exit code