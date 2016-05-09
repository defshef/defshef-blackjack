open System

module Helpers =
    let private rand = new System.Random()

    let shuffle a =
        let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp

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

    type Card =  Card of Suit * Face

    type Deck = Deck of Card list

    type Shoe = Shoe of Card list

    type HandValue =
        | Bust of int
        | BlackJack
        | Value of soft : int * hard : int
        with
            static member IsBustValue =
                function
                | Bust(_) -> true
                | _ -> false
            static member LhsIsGreater lhs rhs =
                if lhs = rhs then
                    false
                else
                    match (lhs, lhs) with
                    | (_, Bust(_)) -> true
                    | (Bust(_), _) -> false
                    | (BlackJack, _) -> true
                    | (_, BlackJack) -> false
                    | (Value(lSoft, lHard), Value(rSoft, rHard)) ->
                        let getActualValue soft hard =
                            if hard > 21 then
                                soft
                            else
                                hard
                        (getActualValue lSoft lHard) > (getActualValue rSoft rHard)

    type Hand = Hand of Card list

    type Play =
        | Hit
        | Stand

    type Player ={
        Id : int
        Hand : Hand
    }

    type GameResult =
        | HouseWins
        | PlayersWin of playerId : Player list

    type Game = {
        Deck : Deck
        Dealer : Hand
        PlayersToGo : Player list
        PlayersFinished : Player list
    }

    [<RequireQualifiedAccess>]
    module Print =
        let cardString (Card(suit, face)) =
            let s =
                match suit with
                | Spade -> "S"
                | Club -> "C"
                | Heart -> "H"
                | Diamond -> "D"
            let f =
                match face with
                | Ace -> "Ace"
                | Number(i) -> i.ToString()
                | Face(p) -> sprintf "%A"  p
            sprintf "(%s %s)" f s

        let handString (Hand(cards)) =
            let cc = cards |> List.map cardString
            String.Join(",", cc)

        let printGame (game : Game) =
            printfn "GAMESTATE:"

            let (Deck(deckCards)) = game.Deck
            printfn "%i cards left in deck" (deckCards.Length)

            printfn "%i players with turns left" (game.PlayersToGo.Length)
            game.PlayersToGo
            |> List.iter (fun p -> printfn "%i : %s" p.Id (handString p.Hand))

            printfn "%i players have hand their turns" (game.PlayersFinished.Length)
            game.PlayersFinished
            |> List.mapi (fun i res -> (i, res))
            |> List.iter (fun (i, res) -> printfn "%i : %A" i res)

            printfn "Dealer Hand: %s" (handString game.Dealer)
            ()

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

    let updateHand (Hand(cards)) (play : Play) deck =
        match play with
        | Stand ->
            (Hand(cards), deck)
        | Hit ->
            let (card, deck') = takeTop deck
            (Hand(card :: cards), deck')

    let playHand (initialHand : Hand) (nextPlay : 'p -> Hand -> (Play * 'p)) prompt (deck : Deck) (initialPlayState : 'p) : (Hand * Deck) =
        let rec playRec hand deck (playState : 'p) =
            let handValue = getHandValue hand
            sprintf "Player hand has a value of %A" handValue |> prompt

            match handValue with
            | Bust(_) | BlackJack ->
                "Player is bust!" |> prompt
                (hand, deck)
            | Value(soft, hard) ->
                let (play, playState') = nextPlay playState hand
                match play with
                | Stand ->
                    "Players Stands" |> prompt
                    (hand, deck)
                | Hit ->
                    let (card, deck') = takeTop deck
                    let (Hand(cards)) = hand
                    let newHand = Hand(card :: cards)
                    sprintf "Player hits, gets a %s" (Print.cardString card) |> prompt
                    playRec newHand deck' playState'

        playRec initialHand deck initialPlayState

    let playPlayers playerPlay prompt game : Game =
        prompt "Player turns"
        let rec playGameRec g =
            match g.PlayersToGo with
            | nextPlayer :: otherPlayers ->
                sprintf "Player %i's turn" nextPlayer.Id |> prompt
                let (finalHand, deck') = playHand nextPlayer.Hand playerPlay prompt g.Deck ()
                let finishedPlayer =
                    { nextPlayer with Hand = finalHand }
                let newGame =
                    { g with
                        PlayersToGo = otherPlayers
                        PlayersFinished = finishedPlayer :: game.PlayersFinished
                        Deck = deck' }
                playGameRec newGame
            | [] ->
                prompt "All players have finished their turns"
                g
        playGameRec game

    let playGame playerPlay dealerPlay prompt game : GameResult =
        let game' = playPlayers playerPlay prompt game

        let allPlayersBust =
            game.PlayersFinished
            |> List.forall (fun p -> p.Hand |> getHandValue |> HandValue.IsBustValue)

        if allPlayersBust then
            prompt "All players are bust"
            HouseWins
        else
            let (dealerResult, deck') = playHand game.Dealer dealerPlay prompt (game'.Deck) ()
            match dealerResult |> getHandValue with
            | BlackJack ->
                HouseWins
            | Bust(_) ->
                let winningPlayers =
                    game'.PlayersFinished
                    |> List.filter (fun p -> p.Hand |> getHandValue |> HandValue.IsBustValue |> not)
                PlayersWin(winningPlayers)
            | Value(_) as v ->
                let winningPlayers =
                    game'.PlayersFinished
                    |> List.filter (fun p -> p.Hand |> getHandValue |> HandValue.LhsIsGreater v |> not)
                PlayersWin(winningPlayers)


    [<EntryPoint>]
    let main argv =
        Console.WriteLine "Write to BackJack"

        let deck = newDeck |> shuffle

        Console.WriteLine "How many players?"
        let line = Console.ReadLine()
        let playerCount = Int32.Parse line

        let (hands, deck) = dealInitialHands (playerCount + 1) deck

        let game = {
            Deck = deck
            Dealer = hands.Head
            PlayersToGo = hands.Tail |> List.mapi (fun i h -> { Id = i; Hand = h})
            PlayersFinished = []
        }

        let playerPlay () hand =
            let rec promptPlay () =
                printf "Current hand:"
                printfn "%s" (Print.handString hand)

                printfn "What do you want to do? Hit or Stand?"
                let p = Console.ReadLine().Trim().ToUpperInvariant()
                match p with
                | "HIT" ->
                    (Hit, ())
                | "STAND" ->
                    (Stand, ())
                | _ ->
                    printfn "Unknown play\r\n"
                    promptPlay ()
            promptPlay ()

        let dealerPlay () hand =
            printf "Dealers hand:"
            printfn "%s" (Print.handString hand)
            match getHandValue hand with
            | Value(soft, hard) when hard <= 17 ->
                printfn "Dealer hits"
                (Hit, ())
            | _ ->
                printfn "Dealer stands"
                (Stand, ())

        let prompt msg =
            printfn "%s" msg

        printf ""
        Print.printGame game

        let gameResult = playGame playerPlay dealerPlay prompt game

        printfn "Game over:"

        match gameResult with
        | HouseWins ->
            printfn "House wins!"
        | PlayersWin(players)->
            printfn "%i players win!" players.Length
            for p in players do
                printfn "- %i" p.Id

        0