module Jasskell.Card where

import           Data.Set                       ( Set )
import           Jasskell.Card.Suit
import           Jasskell.Card.Rank
import           Jasskell.Variant
import           Text.Read

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord)

instance Show Card where
    show c = show (suit c) ++ " " ++ show (rank c)

instance Read Card where
    readPrec = Card <$> readPrec <*> readPrec

type Cards = Set Card

puur :: Suit -> Card
puur s = Card { suit = s, rank = Under }

nell :: Suit -> Card
nell s = Card { suit = s, rank = Nine }

allCards :: [Card]
allCards =
    [ Card { suit = s, rank = r } | s <- [Bells .. Leaves], r <- [Six .. Ace] ]

compareCard :: Variant -> Suit -> Card -> Card -> Ordering
compareCard var lead c1 c2 | c1 == c2              = EQ
                           | cardGT var lead c1 c2 = GT
                           | otherwise             = LT

cardGT :: Variant -> Suit -> Card -> Card -> Bool
cardGT (Trump trump) lead c1 c2 = if suit c1 == trump
    then
        (suit c2 /= trump)
        || (c1 == puur trump)
        || (c1 == nell trump && c2 /= puur trump)
        || (rank c1 > rank c2)
    else suit c2 /= trump && cardGT (Direction TopDown) lead c1 c2
cardGT (Slalom dir) lead c1 c2 = cardGT (Direction dir) lead c1 c2
cardGT (Direction dir) lead (Card s1 r1) (Card s2 r2) =
    (s1 == lead && s2 /= lead) || (s1 == lead || s2 /= lead) && case dir of
        TopDown  -> r1 > r2
        BottomUp -> r1 < r2
