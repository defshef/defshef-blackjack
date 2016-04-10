module Blackjack
    ( shuffledDeck
    , cards
    , value
    ) where

import Text.Regex.Posix ((=~))
import Data.List.Split (endBy)
import System.Random.Shuffle as Shuffle

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Enum)
data Rank = Pip Int | Jack | Queen | King | Ace deriving (Eq)
data Card = Card Rank Suit deriving (Show, Eq)

type Hand = [Card]
type Deck = [Card]

data HandQualifier = None | Soft | Hard | Bust | Blackjack deriving (Show, Eq)
data HandValue = HandValue Int HandQualifier deriving (Show, Eq)

data Game = Game { deck :: Deck
                 , player :: Hand
                 , dealer :: Hand
                 } deriving (Show, Eq)

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

isAce :: Card -> Bool
isAce (Card Ace _) = True
isAce _ = False

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
-- HandValue 12 Hard
-- >>> value (cards "10H QC AS")
-- HandValue 21 Hard
value :: Hand -> HandValue
value hand = HandValue total qualifier
    where
        initialTotal = sum $ map cardValue hand
        ace = any isAce hand
        hardAce = ace && initialTotal > 21
        total
            | hardAce = initialTotal - 10
            | otherwise = initialTotal
        pair = length hand == 2
        qualifier
            | total > 21 = Bust
            | total == 21 && pair = Blackjack
            | hardAce = Hard
            | ace = Soft
            | otherwise = None


-- | Calculate card value
cardValue :: Card -> Int
cardValue (Card (Pip n) _) = n
cardValue (Card rank _)
    | rank == Ace = 11
    | otherwise = 10

-- | The whole deck
freshDeck :: Deck
freshDeck = [Card r s | s <- [(Hearts)..Spades], r <- [Pip 2..Ace]]

-- | Make a new shuffled deck
shuffledDeck :: IO Deck
shuffledDeck = Shuffle.shuffleM freshDeck

-- | Start a game
-- >>> deal $ cards "AS 4D QH 6C 7H 3C"
-- Game {deck = [Card 7 H,Card 3 C], player = [Card A S,Card Q H], dealer = [Card 4 D,Card 6 C]}
deal :: Deck -> Game
deal (a:b:c:d:leftover) = Game { deck=leftover
                               , player=[a, c]
                               , dealer=[b, d] }
