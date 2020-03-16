module Jasskell.Card where

import           Data.Set                       ( Set )
import           Jasskell.Card.Suit
import           Jasskell.Card.Rank
import           Jasskell.Variant

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord, Show)

type Cards = Set Card

puur :: Suit -> Card
puur s = Card { suit = s, rank = Under }

nell :: Suit -> Card
nell s = Card { suit = s, rank = Nine }

allCards :: [Card]
allCards =
    [ Card { suit = s, rank = r } | s <- [Bells .. Leaves], r <- [Six .. Ace] ]

compareCard :: Variant -> Suit -> Card -> Card -> Ordering
compareCard _ _ c1 c2 | c1 == c2 = EQ
compareCard (Trump trump) lead c1 c2
    | c1 == puur trump                     = GT
    | c2 == puur trump                     = LT
    | c1 == nell trump                     = GT
    | c2 == nell trump                     = LT
    | suit c1 == trump && suit c2 /= trump = GT
    | suit c1 /= trump && suit c2 == trump = LT
    | otherwise = compareCard (Direction TopDown) lead c1 c2
compareCard _ lead c1 c2 | suit c1 == lead && suit c2 /= lead = GT
                         | suit c1 /= lead && suit c2 == lead = LT
compareCard (Direction TopDown) _ c1 c2 = compare (rank c1) (rank c2)
compareCard (Direction BottomUp) _ c1 c2 = compare (rank c2) (rank c1)
compareCard (Slalom dir) lead c1 c2 = compareCard (Direction dir) lead c1 c2
