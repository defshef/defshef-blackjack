module Blackjack
    ( shuffledDeck
    , cards
    , value
    ) where

import Text.Regex.Posix ((=~))
import Text.Read (readMaybe)
import Data.List.Split (endBy)
import System.Random.Shuffle as Shuffle

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Enum)
data Rank = Pip Int | Jack | Queen | King | Ace deriving (Eq)
data Card = Card Rank Suit deriving (Show, Eq)

type Hand = [Card]
type Deck = [Card]

data HandQualifier = None | Soft | Hard | Bust | Blackjack deriving (Show, Eq)
data HandValue = HandValue Int HandQualifier deriving (Show, Eq)

data Stage = Player | Dealer | Done deriving (Show, Eq)
data Game = Game { deck :: Deck
                 , player :: Hand
                 , dealer :: Hand
                 , stage :: Stage
                 } deriving (Show, Eq)
data Winner = PlayerWins | DealerWins | Draw deriving (Show, Eq)

instance Show Suit where
    show Hearts = "H"
    show Clubs = "C"
    show Diamonds = "D"
    show Spades = "S"

instance Show Rank where
    show (Pip n) = show n
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

instance Enum Rank where
    fromEnum (Pip n) = n
    fromEnum Jack = 11
    fromEnum Queen = 12
    fromEnum King = 13
    fromEnum Ace = 14
    toEnum n
        | n <= 10 = Pip n
        | n == 11 = Jack
        | n == 12 = Queen
        | n == 13 = King
        | n == 14 = Ace

-- | Build a card
--
-- >>> card Ace Hearts
-- Card A H
--
-- >>> card (Pip 10) Diamonds
-- Card 10 D
--
-- >>> card (Pip 20) Clubs
-- *** Exception: ...
--
-- >>> card (Pip 1) Clubs
-- *** Exception: ...
--
card :: Rank -> Suit -> Card
card (Pip n) s
    | n < 2 = error "Rank too low"
    | n > 10 = error "Rank too high"
card r s = Card r s

-- | Helper function for making a sequence of cards
--
-- I'm not entirely convinced that writing this was worth it.
-- Oh well.
--
-- >>> cards "AH KS"
-- [Card A H,Card K S]
cards :: String -> [Card]
cards = (map parseCard) . (endBy " ")

-- | Parse a card string into a Card
-- >>> parseCard "AH"
-- Card A H
-- >>> parseCard "6S"
-- Card 6 S
parseCard :: String -> Card
parseCard str = Card (parseRank r) (parseSuit s)
    where
        [[_, r, s]] = str =~ "^([2-9]|10|[JQKA])([HCDS])$" :: [[String]]

parseRank :: String -> Rank
parseRank "A" = Ace
parseRank "K" = King
parseRank "Q" = Queen
parseRank "J" = Jack
parseRank i = Pip (read i)

parseSuit :: String -> Suit
parseSuit "H" = Hearts
parseSuit "C" = Clubs
parseSuit "D" = Diamonds
parseSuit "S" = Spades

------------------------------------
--- Calculate hand value
------------------------------------

-- | Calculate hand value
--
-- >>> value (cards "2H 2C")
-- HandValue 4 None
-- >>> value (cards "2H 2C 2D 2S")
-- HandValue 8 None
-- >>> value (cards "2H 2C 7S 8D")
-- HandValue 19 None
-- >>> value (cards "KH 5C 6S")
-- HandValue 21 None
-- >>> value (cards "KH QC 3S")
-- HandValue 23 Bust
-- >>> value (cards "9H AC 3S KS")
-- HandValue 23 Bust
-- >>> value (cards "JH 8C 9S")
-- HandValue 27 Bust
-- >>> value (cards "AS KS")
-- HandValue 21 Blackjack
-- >>> value (cards "AS 5H")
-- HandValue 16 Soft
-- >>> value (cards "AS 5H 7C")
-- HandValue 13 Hard
-- >>> value (cards "AS AC")
-- HandValue 12 Soft
-- >>> value (cards "AS AC AH AD")
-- HandValue 14 Soft
-- >>> value (cards "AS AC KH")
-- HandValue 12 Hard
-- >>> value (cards "AS AC AH AD KC")
-- HandValue 14 Hard
-- >>> value (cards "AS AC AH AD KC JD")
-- HandValue 24 Bust
-- >>> value (cards "10H QC AS")
-- HandValue 21 Hard
value :: Hand -> HandValue
value hand = HandValue total qualifier
    where
        initialTotal = sum $ map cardValue hand
        ace = any isAce hand
        soft = ace && initialTotal <= 11
        total
            | soft = initialTotal + 10
            | otherwise = initialTotal
        pair = length hand == 2
        qualifier
            | total > 21 = Bust
            | total == 21 && pair = Blackjack
            | soft = Soft
            | ace = Hard
            | otherwise = None

-- | Calculate card value
cardValue :: Card -> Int
cardValue (Card (Pip n) _) = n
cardValue (Card rank _)
    | rank == Ace = 1
    | otherwise = 10

isAce :: Card -> Bool
isAce (Card Ace _) = True
isAce _ = False

------------------------------------
--- Shuffle & deal
------------------------------------

-- | The whole deck
freshDeck :: Deck
freshDeck = [Card r s | s <- [(Hearts)..Spades], r <- [Pip 2..Ace]]

-- | Make a new shuffled deck
shuffledDeck :: IO Deck
shuffledDeck = Shuffle.shuffleM freshDeck

-- | Start a game
-- >>> deal $ cards "AS 4D QH 6C 7H 3C"
-- Game {deck = [Card 7 H,Card 3 C], player = [Card A S,Card Q H], dealer = [Card 4 D,Card 6 C], stage = Player}
deal :: Deck -> Game
deal (a:b:c:d:leftover) = Game { deck=leftover
                               , player=[a, c]
                               , dealer=[b, d]
                               , stage=Player }


------------------------------------
--- Hit / Stand & basic gameplay
------------------------------------

-- | Hit
--
-- Takes another card:
-- >>> hit Game {deck = cards "AC", player = cards "6C 10D", dealer = cards "7S KC", stage = Player}
-- Game {deck = [], player = [Card 6 C,Card 10 D,Card A C], dealer = [Card 7 S,Card K C], stage = Player}
--
-- Moves to dealer if bust:
-- >>> hit Game {deck = cards "10C", player = cards "6C 10D", dealer = cards "7S KC", stage = Player}
-- Game {deck = [], player = [Card 6 C,Card 10 D,Card 10 C], dealer = [Card 7 S,Card K C], stage = Dealer}
hit :: Game -> Game
hit game = game { deck=remaining, player=player', stage=stage' }
    where
        (card:remaining) = deck game
        player' = player game ++ [card]
        HandValue _ qualifier = value player'
        stage'
            | qualifier == Bust = Dealer
            | otherwise = stage game

-- | Stand
--
-- Stops taking cards
-- >>> stand Game { deck = cards "AC", player = cards "8C 10D", dealer = cards "7S KC", stage = Player}
-- Game {deck = [Card A C], player = [Card 8 C,Card 10 D], dealer = [Card 7 S,Card K C], stage = Dealer}
stand :: Game -> Game
stand game = game { stage=Dealer }

-- | Play dealer
--
-- Dealer stands on 17
-- >>> playDealer Game {deck = cards "AC", player = cards "8C 10D", dealer = cards "7S KC", stage = Dealer}
-- Game {deck = [Card A C], player = [Card 8 C,Card 10 D], dealer = [Card 7 S,Card K C], stage = Done}
--
-- Dealer plays on under 17
-- >>> playDealer Game {deck = cards "6C", player = cards "8C 10D", dealer = cards "5S KC", stage = Dealer}
-- Game {deck = [], player = [Card 8 C,Card 10 D], dealer = [Card 5 S,Card K C,Card 6 C], stage = Done}
--
-- Dealer keeps playing until over 17
-- >>> playDealer Game {deck = cards "3C 4D 2S JD 7D", player = cards "8C 10D", dealer = cards "4S 3D", stage = Dealer}
-- Game {deck = [Card 7 D], player = [Card 8 C,Card 10 D], dealer = [Card 4 S,Card 3 D,Card 3 C,Card 4 D,Card 2 S,Card J D], stage = Done}
playDealer :: Game -> Game
playDealer game
    | total < 17 = playDealer $ game {deck=deck', dealer=dealer', stage=Dealer}
    | otherwise = game {deck=deck game, dealer=dealer game, stage=Done}
    where
        HandValue total _ = value $ dealer game
        (card:deck') = deck game
        dealer' = dealer game ++ [card]

-- | Who won?
--
-- not bust, dealer higher
-- >>> whoWon Game {deck = [], player = cards "8C 10D", dealer = cards "JS KC", stage = Done}
-- DealerWins
--
-- not bust, player higher
-- >>> whoWon Game {deck = [], player = cards "10C 10D", dealer = cards "9S KC", stage = Done}
-- PlayerWins
--
-- not bust, same score
-- >>> whoWon Game {deck = [], player = cards "10C 9D", dealer = cards "9S KC", stage = Done}
-- Draw
--
-- dealer bust
-- >>> whoWon Game {deck = [], player = cards "10C 9D", dealer = cards "6S KC JD", stage = Done}
-- PlayerWins
--
-- player bust
-- >>> whoWon Game {deck = [], player = cards "10C 5D 8C", dealer = cards "7S KC", stage = Done}
-- DealerWins
--
-- both bust
-- >>> whoWon Game {deck = [], player = cards "10C 5D 8C", dealer = cards "7S KC", stage = Done}
-- DealerWins
--
-- Blackjack
-- >>> whoWon Game {deck = [], player = cards "10C AC", dealer = cards "6S 5D KC", stage = Done}
-- PlayerWins
--
-- Dealer Blackjack
-- >>> whoWon Game {deck = [], player = cards "6S 5D KC", dealer = cards "10C AC", stage = Done}
-- DealerWins
--
-- Both Blackjack
-- >>> whoWon Game {deck = [], player = cards "AS KC", dealer = cards "10C AC", stage = Done}
-- Draw
whoWon :: Game -> Winner
whoWon game
    | pQualifier == Bust = DealerWins
    | dQualifier == Bust = PlayerWins
    | pQualifier == Blackjack && dQualifier == Blackjack = Draw
    | pQualifier == Blackjack = PlayerWins
    | dQualifier == Blackjack = DealerWins
    | pTotal > dTotal = PlayerWins
    | dTotal > pTotal = DealerWins
    | pTotal == dTotal = Draw
    where
        HandValue pTotal pQualifier = value $ player game
        HandValue dTotal dQualifier = value $ dealer game

------------------------------------
--- Display the game
------------------------------------


-- | Render the game to string
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 5D", player = cards "9D 8H", stage = Player }
-- Dealer: XX 5D
-- Player: 9D 8H (17)
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 6S", player = cards "AD 8H", stage = Player }
-- Dealer: XX 6S
-- Player: AD 8H (soft 19)
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 6S", player = cards "AD 8H 7D", stage = Player }
-- Dealer: XX 6S
-- Player: AD 8H 7D (hard 16)
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 6S", player = cards "AD 10H", stage = Player }
-- Dealer: XX 6S
-- Player: AD 10H (blackjack!)
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 6S", player = cards "6D 10H KC", stage = Player }
-- Dealer: XX 6S
-- Player: 6D 10H KC (26 BUST)
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 7S", player = cards "6D 10H KC", stage = Done }
-- Dealer: KH 7S (17)
-- Player: 6D 10H KC (26 BUST)
-- Dealer wins
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "KH 6S 8H", player = cards "6D 10H", stage = Done }
-- Dealer: KH 6S 8H (24 BUST)
-- Player: 6D 10H (16)
-- Player wins
--
-- >>> putStrLn $ render Game { deck = cards "AC", dealer = cards "9H AH", player = cards "6D 4C 10H", stage = Done }
-- Dealer: 9H AH (soft 20)
-- Player: 6D 4C 10H (20)
-- Draw
render :: Game -> String
render game =
    "Dealer: " ++ (renderDealer $ dealer game) ++ "\n" ++
    "Player: " ++ (renderHand $ player game) ++
    winLine
    where
        winLine = case stage game of
            Player -> ""
            _ -> "\n" ++ winner
        winner = case whoWon game of
            PlayerWins -> "Player wins"
            DealerWins -> "Dealer wins"
            _ -> "Draw"
        renderDealer = case stage game of
            Player -> renderObscured
            _ -> renderHand

renderHand :: Hand -> String
renderHand hand = cards ++ " (" ++ score ++ ")"
    where
        cards = unwords $ map renderCard hand
        HandValue total qualifier = value hand
        score = case qualifier of
            Blackjack -> "blackjack!"
            Bust -> show total ++ " BUST"
            Soft -> "soft " ++ show total
            Hard -> "hard " ++ show total
            _ -> show total

renderObscured :: Hand -> String
renderObscured (_:hand) = "XX " ++ (unwords $ map renderCard hand)

renderCard :: Card -> String
renderCard (Card r s) = show r ++ show s

------------------------------------
--- Tying it all together
------------------------------------

data Action = Hit | Stand deriving (Show, Read, Eq)
actualAction :: Action -> (Game -> Game)
actualAction Hit = hit
actualAction Stand = stand

playGame :: IO ()
playGame = do
    deck <- shuffledDeck
    gameLoop $ deal deck

gameLoop :: Game -> IO ()
gameLoop game = do
    putStrLn $ render game
    let actions = possibleActions game
    if null actions then do
        putStrLn $ render $ playDealer game
    else do
        actionName <- acceptAction actions
        let action = actualAction actionName
        gameLoop $ action game

acceptAction :: [Action] -> IO Action
acceptAction actions = do
    putStrLn $ "Choose one of " ++
        (unwords $ map show actions)
    actionStr <- getLine
    case readMaybe actionStr of
        Just action -> return action
        Nothing -> acceptAction actions

possibleActions :: Game -> [Action]
possibleActions game = case stage game of
    Player -> case value $ player game of
        HandValue _ Bust -> [Stand]
        HandValue _ Blackjack -> [Stand]
        HandValue _ _ -> [Hit, Stand]
    _ -> []
